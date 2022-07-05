package com.github.prologdb.runtime.stdlib.essential

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinNot = nativeRule("not", 1) { args, context ->
    val arg0 = args[0] as? CompoundTerm ?: return@nativeRule null

    val proofSequence = buildLazySequence<Unification>(context.principal) {
        context.fulfillAttach(this, PredicateInvocationQuery(arg0), VariableBucket())
    }

    val hasProof = proofSequence.tryAdvance() != null
    proofSequence.close()

    return@nativeRule Unification.whether(!hasProof) // this is the core logic here
}

val BuiltinNotOperator = nativeRule("\\+", 1) { args, context ->
    BuiltinNot.fulfill(this, args.raw, context)
}

val BuiltinUnity = nativeRule("=", 2) { args, context ->
    args[0].unify(args[1], context.randomVariableScope)
}

val BuiltinNegatedUnity = nativeRule("\\=", 2) { args, context ->
    Unification.whether(args[0].unify(args[1], context.randomVariableScope) == Unification.FALSE)
}

val BuiltinIdentity = nativeRule("==", 2) { args, _ ->
    Unification.whether(args[0] == args[1])
}

val BuiltinNegatedIdentityOperator = nativeRule("\\==", 2) { args, _ ->
    Unification.whether(args[0] != args[1])
}
