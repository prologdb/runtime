package com.github.prologdb.runtime.builtin.dynamic

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.optimization.TailcallOptimizer
import com.github.prologdb.runtime.proofsearch.ASTPrologPredicate
import com.github.prologdb.runtime.proofsearch.DelegatableCallable
import com.github.prologdb.runtime.proofsearch.DynamicPrologPredicate
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.unification.Unification

private val tailcallOptimizer = TailcallOptimizer()

internal val BuiltinCompilePredicates = nativeRule("compile_predicates", 1) { args, context ->
    val arg0 = args[0]
    if (arg0 !is PrologList) {
        throw PrologRuntimeException("Argument 1 to compile_predicates/1 must be a list, ${arg0.prologTypeName} given")
    }

    if (arg0.tail != null) {
        throw PrologRuntimeException("Argument 1 to compile_predicates/1 must not have a tail")
    }

    val fqIndicatorAndCallables: Sequence<Pair<FullyQualifiedClauseIndicator, PrologCallable>> = arg0.elements
        .asSequence()
        .mapIndexed { index, term ->
            val simpleIndicator = ClauseIndicator.ofIdiomatic(term, "element ${index + 1} in argument 1 to compile_predicates/1")
            context.resolveCallable(simpleIndicator)
                ?: throw PrologRuntimeException("Predicate $simpleIndicator is not defined")
        }

    fqIndicatorAndCallables.forEach {
        if (it.second !is DelegatableCallable) {
            throw PrologRuntimeException("Cannot optimize and/or compile ${it.first}: not an instance of ${DelegatableCallable::class.java.name}")
        }
    }

    fqIndicatorAndCallables.forEach { (_, callable) ->
        if (callable is DynamicPrologPredicate) {
            callable.seal()
        }
    }

    for ((fqIndicator, callable) in fqIndicatorAndCallables) {
        if (callable is ASTPrologPredicate) {
            tailcallOptimizer.tryOptimize(callable, context.runtime)?.let { optimized ->
                callable.setDelegate(optimized, true)
            }
        }
    }

    yield(Unification.TRUE)
}
