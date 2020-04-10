package com.github.prologdb.runtime.builtin

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.analyzation.constraint.InvocationBehaviour
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinNot = nativeRule("not", 1) { args, context ->
    val arg0 = args[0] as? CompoundTerm ?: return@nativeRule

    val proofSequence = buildLazySequence<Unification>(context.principal) {
        context.fulfillAttach(this, PredicateInvocationQuery(arg0), VariableBucket())
    }

    val hasProof = proofSequence.tryAdvance() != null
    proofSequence.close()

    if (!hasProof) yield(Unification.TRUE) // this is the core logic here
}.apply {

}

val BuiltinNotOperator = nativeRule("\\+", 1) { args, context ->
    BuiltinNot.fulfill(this, args, context)
}.apply {

}

val BuiltinUnity = nativeRule("=", 2) { args, context ->
    args[0].unify(args[1], context.randomVariableScope)?.let { yield(it) }
}.apply {
    addDeterministicBehaviour(
        InvocationBehaviour.unifiesWith(
            CompoundTerm("=", arrayOf(builtinArgumentVariables[0], builtinArgumentVariables[0]))
        )
    )
}

val BuiltinNegatedUnity = nativeRule("\\=", 2) { args, context ->
    if (args[0].unify(args[1], context.randomVariableScope) == Unification.FALSE) {
        yield(Unification.TRUE)
    }
}.apply {

}

val BuiltinIdentity = nativeRule("==", 2) { args, _ ->
    if (args[0] == args[1]) yield(Unification.TRUE)
}.apply {

}

val BuiltinNegatedIdentityOperator = nativeRule("\\==", 2) { args, _ ->
    if (args[0] != args[1]) yield(Unification.TRUE)
}.apply {

}

/**
 * Defines the ISO equality and inequality predicates and operators.
 */
val EqualityModule = nativeModule("equality") {
    add(BuiltinUnity)
    add(BuiltinNegatedUnity)
    add(BuiltinNot)
    add(BuiltinNotOperator)
    add(BuiltinIdentity)
    add(BuiltinNegatedIdentityOperator)
}
