package com.github.prologdb.runtime.stdlib.solution_sequences

import com.github.prologdb.async.*
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.numberVariables
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableDiscrepancyException
import java.util.Collections
import java.util.concurrent.ConcurrentHashMap

val BuiltinGroupBy4 = nativeRule("group_by", 4) { args, ctxt ->
    val groupingBy = args[0]
    val template = args[1]
    val goal = args.getQuery(2)
    val bagInput = args[3]

    val bagsByGrouping = await(
        buildLazySequence(ctxt.principal) {
            ctxt.fulfillAttach(this, goal, Unification())
        }
            .foldRemaining(ConcurrentHashMap<KeyIdentity<Unification, Term>, MutableList<Term>>()) { bagByGrouping, solution ->
                val normalizedGrouping = groupingBy
                    .substituteVariables(solution.variableValues.asSubstitutionMapper())
                    .numberVariables()
                val bagKey = KeyIdentity(solution, normalizedGrouping)

                val instantiatedTemplate = template.substituteVariables(solution.variableValues.asSubstitutionMapper())

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

                return@mapRemainingNotNull try {
                    solution.combinedWith(bagUnification, ctxt.randomVariableScope)
                }
                catch (ex: VariableDiscrepancyException) {
                    null
                }
            }
    )
}