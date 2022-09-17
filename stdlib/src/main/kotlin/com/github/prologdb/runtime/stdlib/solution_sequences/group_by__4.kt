package com.github.prologdb.runtime.stdlib.solution_sequences

import com.github.prologdb.async.KeyIdentity
import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.foldRemaining
import com.github.prologdb.async.mapRemainingNotNull
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.numberVariables
import com.github.prologdb.runtime.unification.Unification
import java.util.Collections
import java.util.concurrent.ConcurrentHashMap

val BuiltinGroupBy4 = nativeRule("group_by", 4) { args, ctxt ->
    val groupingBy = args[0]
    val template = args[1]
    val goal = args.getQuery(2)
    val bagInput = args[3]

    val bagsByGrouping = await(
        buildLazySequence(ctxt.principal) {
            ctxt.fulfillAttach(this, goal, Unification.TRUE)
        }
            .foldRemaining(ConcurrentHashMap<KeyIdentity<Unification, Term>, MutableList<Term>>()) { bagByGrouping, solution ->
                val normalizedGrouping = groupingBy
                    .substituteVariables(solution.asSubstitutionMapper())
                    .numberVariables()
                val bagKey = KeyIdentity(solution, normalizedGrouping)

                val instantiatedTemplate = template.substituteVariables(solution.asSubstitutionMapper())

                val bag = bagByGrouping.computeIfAbsent(bagKey) { Collections.synchronizedList(ArrayList()) }
                bag.add(instantiatedTemplate)

                bagByGrouping
            }
    )

    yieldAllFinal(
        LazySequence.ofIterable(bagsByGrouping.entries)
            .mapRemainingNotNull { (key, instantiatedBag) ->
                val solution = key.value
                val bagUnification = bagInput.unify(PrologList(instantiatedBag), ctxt.randomVariableScope)
                    ?: return@mapRemainingNotNull null

                return@mapRemainingNotNull solution.combinedWith(bagUnification, ctxt.randomVariableScope)
            }
    )
}