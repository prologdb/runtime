import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.async.remainingTo
import com.github.prologdb.parser.PredefinedSourceModuleLoader
import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.LineEndingNormalizer
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.DefaultPrologRuntimeEnvironment
import com.github.prologdb.runtime.module.CascadingModuleLoader
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.playground.jvm.CharacterIterable
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

fun main() {
    val moduleSourceCode = """
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
    // this allows the runtime to load our custom source code
    val userModuleReference = ModuleReference("prog", "user")
    val userModuleLoader = PredefinedSourceModuleLoader().apply {
        registerModule(userModuleReference, moduleSourceCode)
    }

    // this adds the standard library to the runtime
    val moduleLoader = CascadingModuleLoader(userModuleLoader, ModuleLoader.discoverOnClasspath())

    // this initializes the runtime
    val runtime = DefaultPrologRuntimeEnvironment(moduleLoader)
    val loadedUserModule = runtime.assureModuleLoaded(userModuleReference)

    val queryCode = """jealous(marcellus, Person)."""
    val queryLexer = Lexer(
        SourceUnit("query"),
        LineEndingNormalizer(CharacterIterable(queryCode).iterator())
    )
    val queryParseResult = PrologParser().parseQuery(queryLexer, loadedUserModule.localOperators)
    if (queryParseResult.reportings.isNotEmpty()) {
        // parsing not successful
    }
    val queryAST = queryParseResult.item!!

    val solutions: LazySequence<Unification> = runtime.fulfill(userModuleReference.moduleName, queryAST)
    val personsJealousOfMarcellus = solutions
        .mapRemaining { it[Variable("Person")] }
        .mapRemaining { (it as Atom).name }
        .remainingTo(::mutableSetOf)

    println(personsJealousOfMarcellus)
}