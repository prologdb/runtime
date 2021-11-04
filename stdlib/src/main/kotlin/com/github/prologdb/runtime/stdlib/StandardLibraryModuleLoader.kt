package com.github.prologdb.runtime.stdlib

import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.LineEndingNormalizer
import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleNotFoundException
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.stdlib.dicts.BuiltinGetDict3
import com.github.prologdb.runtime.stdlib.essential.BuiltinAtom1
import com.github.prologdb.runtime.stdlib.essential.BuiltinDecimal1
import com.github.prologdb.runtime.stdlib.essential.BuiltinGround1
import com.github.prologdb.runtime.stdlib.essential.BuiltinInteger1
import com.github.prologdb.runtime.stdlib.essential.BuiltinIsDict1
import com.github.prologdb.runtime.stdlib.essential.BuiltinIsList1
import com.github.prologdb.runtime.stdlib.essential.BuiltinNonGround1
import com.github.prologdb.runtime.stdlib.essential.BuiltinNonVar1
import com.github.prologdb.runtime.stdlib.essential.BuiltinNumber1
import com.github.prologdb.runtime.stdlib.essential.BuiltinString1
import com.github.prologdb.runtime.stdlib.essential.BuiltinTypeof2
import com.github.prologdb.runtime.stdlib.essential.BuiltinVar1
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinAbolish1
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinAssert1
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinRetract1
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinRetractAll1
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinApply2
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinCall1
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinCompoundNameArguments3
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinCurrentOp3
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinFindAll3
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinFindAllOptimized3
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinFindNSols4
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinTermVariables2
import com.github.prologdb.runtime.stdlib.essential.math.BuiltinGreaterThan2
import com.github.prologdb.runtime.stdlib.essential.math.BuiltinGreaterThanOrEqual2
import com.github.prologdb.runtime.stdlib.essential.math.BuiltinIs2
import com.github.prologdb.runtime.stdlib.essential.math.BuiltinLessThan2
import com.github.prologdb.runtime.stdlib.essential.math.BuiltinLessThanOrEqual2
import com.github.prologdb.runtime.stdlib.essential.string.BuiltinAtomString2
import com.github.prologdb.runtime.stdlib.essential.string.BuiltinStringChars2
import com.github.prologdb.runtime.stdlib.essential.string.BuiltinStringCodes2
import com.github.prologdb.runtime.stdlib.lists.BuiltinIota3
import com.github.prologdb.runtime.stdlib.lists.BuiltinIota4
import com.github.prologdb.runtime.stdlib.lists.BuiltinLength2
import com.github.prologdb.runtime.stdlib.lists.BuiltinMember2
import com.github.prologdb.runtime.stdlib.lists.BuiltinSet2
import com.github.prologdb.runtime.stdlib.lists.BuiltinSet3
import com.github.prologdb.runtime.stdlib.lists.BuiltinSort2
import com.github.prologdb.runtime.stdlib.sort.BuiltinPredsort3
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorType
import java.nio.file.Paths

object StandardLibraryModuleLoader : ModuleLoader {
    private val _parser = PrologParser()

    override fun load(reference: ModuleReference): Module {
        val sourceFileUrl = StandardLibraryModuleLoader::class.java.classLoader.getResource(
            Paths.get(reference.pathAlias).resolve(reference.moduleName + ".pl").toString()
        )
            ?: throw ModuleNotFoundException(reference)

        val sourceText = sourceFileUrl.readText(Charsets.UTF_8)
        val result = _parser.parseSourceFile(
            Lexer(
                SourceUnit(sourceFileUrl.toString()),
                LineEndingNormalizer(sourceText.iterator())
            ),
            NativeCodeSupplyingSourceFileVisitor(reference)
        )

        result.reportings.firstOrNull()?.let {
            throw PrologRuntimeException("Failed to load module $reference: $it (and ${result.reportings.size - 1} more)")
        }

        val module = result.item!!

        if (module.name != reference.moduleName) {
            throw PrologRuntimeException("Source for module $reference declares a different module name (${module.name}).")
        }

        return module
    }

    private class NativeCodeSupplyingSourceFileVisitor(
        private val moduleReference: ModuleReference
    ) : DefaultModuleSourceFileVisitor(null, emptySet()) {
        private val nativeImplementations: Map<ClauseIndicator, NativeCodeRule> =
            nativeImplementationsByModuleRef[moduleReference.toString()] ?: emptyMap()

        init {
            operators.defineOperator(OperatorDefinition(1150, OperatorType.FX, "native"))
        }

        private val declaredNative = mutableSetOf<ClauseIndicator>()

        override fun visitDirective(command: CompoundTerm): Collection<Reporting> {
            if (command.functor == "native" && command.arity == 1) {
                val clauseIndicatorResult = _parser.parseIdiomaticClauseIndicator(command.arguments[0])
                val reportings = clauseIndicatorResult.item
                    ?.let { this.visitNativeImport(it, command.sourceInformation as SourceLocation) }
                    ?: emptySet()

                return reportings + clauseIndicatorResult.reportings
            }

            return super.visitDirective(command)
        }

        private fun visitNativeImport(indicator: ClauseIndicator, location: SourceLocation): Collection<Reporting> {
            val nativeCode = nativeImplementations[indicator]
                ?: return setOf(SemanticError(
                    "Did not find a native implementation for $indicator in $moduleReference",
                    location
                ))

            if (declaredNative.add(indicator)) {
                visitClause(nativeCode, location)
                return emptySet()
            } else {
                return setOf(SemanticError("$indicator declared native twice", location))
            }
        }

        companion object {
            private val nativeImplementationsByModuleRef: Map<String, Map<ClauseIndicator, NativeCodeRule>> = mapOf(
                "essential(\$equality)" to listOf(
                    BuiltinUnity,
                    BuiltinNegatedUnity,
                    BuiltinNot,
                    BuiltinNotOperator,
                    BuiltinIdentity,
                    BuiltinNegatedIdentityOperator
                ),
                "essential(\$typesafety)" to listOf(
                    BuiltinAtom1,
                    BuiltinInteger1,
                    BuiltinDecimal1,
                    BuiltinNumber1,
                    BuiltinString1,
                    BuiltinIsList1,
                    BuiltinIsDict1,
                    BuiltinVar1,
                    BuiltinNonVar1,
                    BuiltinGround1,
                    BuiltinNonGround1,
                    BuiltinTypeof2
                ),
                "essential(\$comparison)" to listOf(
                    BuiltinTermLessThan2,
                    BuiltinTermLessThanOrEqual2,
                    BuiltinTermGreaterThan2,
                    BuiltinTermGreaterThanOrEqual2,
                    BuiltinCompare3
                ),
                "essential(\$dynamic)" to listOf(
                    BuiltinFindAll3,
                    BuiltinFindAllOptimized3,
                    BuiltinFindNSols4,
                    BuiltinCall1,
                    BuiltinCompoundNameArguments3,
                    BuiltinApply2,
                    BuiltinTermVariables2,
                    BuiltinCurrentOp3
                ),
                "essential(\$math)" to listOf(
                    BuiltinIs2,
                    BuiltinLessThan2,
                    BuiltinLessThanOrEqual2,
                    BuiltinGreaterThan2,
                    BuiltinGreaterThanOrEqual2
                ),
                "essential(\$strings)" to listOf(
                    BuiltinAtomString2,
                    BuiltinStringChars2,
                    BuiltinStringCodes2
                ),
                "essential(\$clauses)" to listOf(
                    BuiltinAbolish1,
                    BuiltinAssert1,
                    BuiltinRetract1,
                    BuiltinRetractAll1
                ),
                "library(dicts)" to listOf(
                    BuiltinGetDict3
                ),
                "library(lists)" to listOf(
                    BuiltinIota3,
                    BuiltinIota4,
                    BuiltinLength2,
                    BuiltinMember2,
                    BuiltinSet2,
                    BuiltinSet3,
                    BuiltinSort2
                ),
                "library(sort)" to listOf(
                    BuiltinPredsort3
                )
            ).mapValues { (_, nativeCodes) ->
                nativeCodes.associateBy(ClauseIndicator.Companion::of)
            }
        }
    }
}
