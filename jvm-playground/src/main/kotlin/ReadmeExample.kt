import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.async.remainingTo
import com.github.prologdb.parser.ModuleDeclaration
import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.LineEndingNormalizer
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.playground.jvm.CharacterIterable
import com.github.prologdb.runtime.proofsearch.ReadWriteAuthorization
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

fun main() {
    val moduleSourceCode: String = """
        :- use_module(library(equality)).
        loves(vincent, mia).
        loves(marcellus, mia).
        loves(pumpkin, honey_bunny).
        loves(honey_bunny, pumpkin).
        
        jealous(X, Y) :-
            loves(X, Z),
            loves(Y, Z),
            X \= Y
            .
    """
    val lexer = Lexer(
        SourceUnit("root module"),
        LineEndingNormalizer(
            CharacterIterable(moduleSourceCode).iterator()
        )
    )

    val moduleParseResult = PrologParser().parseModule(lexer, ISOOpsOperatorRegistry, ModuleDeclaration("_root", null))
    if (moduleParseResult.reportings.isNotEmpty()) {
        // parsing not successful
    }

    val module = moduleParseResult.item!!

    val runtime = PrologRuntimeEnvironment(module)

    val queryCode = """jealous(marcellus, Person)."""
    val queryLexer = Lexer(
        SourceUnit("query"),
        LineEndingNormalizer(CharacterIterable(queryCode).iterator())
    )
    val queryParseResult = PrologParser().parseQuery(queryLexer, module.localOperators)
    if (queryParseResult.reportings.isNotEmpty()) {
        // parsing not successful
    }
    val queryAST = queryParseResult.item!!

    val solutions: LazySequence<Unification> = runtime.fulfill(queryAST, ReadWriteAuthorization)
    val personsJealousOfMarcellus = solutions
        .mapRemaining { it.variableValues[Variable("Person")] }
        .mapRemaining { (it as Atom).name }
        .remainingTo(::mutableSetOf)

    println(personsJealousOfMarcellus)
}