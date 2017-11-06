package com.github.tmarsteel.ktprolog.playground.jvm

import com.github.tmarsteel.ktprolog.knowledge.DefaultKnowledgeBase
import com.github.tmarsteel.ktprolog.parser.lexer.Lexer
import com.github.tmarsteel.ktprolog.parser.lexer.LineEndingNormalizer
import com.github.tmarsteel.ktprolog.parser.parser.PrologParser
import com.github.tmarsteel.ktprolog.parser.source.SourceUnit

val kbAsString = """
append([], [H|T], [H|T]).
append([H|T], L, [H|TA]) :- append(T, L, TA).
"""

fun main(args: Array<String>) {
    println("Reading knowledge base...")

    val parser = PrologParser()

    val kb = DefaultKnowledgeBase()

    val result = parser.parseDefinitionsInto(
        Lexer(
            SourceUnit("test.pl"),
            LineEndingNormalizer(kbAsString.iterator())
        ),
        kb
    )

    result.reportings.forEach(::println)

    if (result.reportings.isNotEmpty()) return

    println("Done.")
    println()

    println("Enter query:")
    println()
    print("?- ")
    val queryAsString = readLine()!!

    val queryLexer = Lexer(SourceUnit("query"), LineEndingNormalizer(queryAsString.iterator()))
    val queryResult = parser.parseQuery(queryLexer)

    queryResult.reportings.forEach(::println)

    if (queryResult.reportings.isNotEmpty()) return

    val queryResponse = queryResult.item!!.findProofWithin(kb)

    var hadOne = false
    val start = System.currentTimeMillis()
    val solutions = queryResponse.toList()
    val end = System.currentTimeMillis()

    for (solution in solutions) {
        println("-------")
        if (solution.variableValues.isEmpty) {
            println("true")
        } else {
            println(solution)
        }
        hadOne = true
    }

    if (!hadOne) {
        println("-------")
        println("false")
    }

    println()
    println()
    println("Found all solutions in ${end - start}ms")
}