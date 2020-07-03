import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.module.ASTModule
import com.github.prologdb.runtime.module.NativeLibraryLoader
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification

fun main() {
    val module = ASTModule("root", emptyList(), emptyList(), emptySet(), emptySet())
    val runtime = PrologRuntimeEnvironment(module, NativeLibraryLoader.withCoreLibraries())

    val rule = Rule(
        CompoundTerm("a", emptyArray()),
        AndQuery (emptyArray())
    )

    val psc = runtime.newProofSearchContext()
    val sequence = buildLazySequence<Unification>(psc.principal) {
        rule.fulfill(this, emptyArray(), psc)
    }

    while (sequence.state != LazySequence.State.DEPLETED) {
        println("----")
        println(sequence.state)
        println(sequence.step())
        while (sequence.state == LazySequence.State.RESULTS_AVAILABLE) {
            println("Result: " + sequence.tryAdvance())
            println("-> " + sequence.state)
        }
    }
}