package com.github.prologdb.runtime.expression

import com.github.prologdb.runtime.shouldNotUnifyWith
import com.github.prologdb.runtime.shouldUnifyWith
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.Variable
import io.kotest.core.spec.style.FreeSpec

class DictionaryUnificationTest : FreeSpec() { init {
    "empty" {
        val a = PrologDictionary(emptyMap())
        val b = PrologDictionary(emptyMap())

        a shouldUnifyWith b suchThat {
            it.variableValues.isEmpty
        }
    }

    "both without tail, matching keys and values" {
        val a = Atom("a")
        val dA = PrologDictionary(mapOf(a to a))
        val dB = PrologDictionary(mapOf(a to a))

        dA shouldUnifyWith dB suchThat {
            it.variableValues.isEmpty
        }
    }

    "both without tail, matching keys, discrepant values" {
        val a = Atom("a")
        val b = Atom("b")
        val dA = PrologDictionary(mapOf(a to a))
        val dB = PrologDictionary(mapOf(a to b))

        dA shouldNotUnifyWith dB
    }

    "both without tail, discrepant keys" {
        val a = Atom("a")
        val b = Atom("b")
        val dA = PrologDictionary(mapOf(a to a))
        val dB = PrologDictionary(mapOf(b to b))

        dA shouldNotUnifyWith dB
    }

    "only LHS with tail" - {
        "rhs with extra keys" - {
            "values for common keys match" {
                val a = Atom("a")
                val b = Atom("b")
                val T = Variable("T")
                val dA = PrologDictionary(mapOf(a to a), T)
                val dB = PrologDictionary(mapOf(a to a, b to b))

                dA shouldUnifyWith dB suchThat {
                    it.variableValues[T] == PrologDictionary(mapOf(b to b))
                }
            }

            "values for common keys discrepant" {
                val a = Atom("a")
                val b = Atom("b")
                val T = Variable("T")
                val dA = PrologDictionary(mapOf(a to a), T)
                val dB = PrologDictionary(mapOf(a to b, b to b))

                dA shouldNotUnifyWith dB
            }
        }

        "rhs with same keys" - {
            "matching values" {
                val a = Atom("a")
                val T = Variable("T")
                val dA = PrologDictionary(mapOf(a to a), T)
                val dB = PrologDictionary(mapOf(a to a))

                dA shouldUnifyWith dB suchThat {
                    it.variableValues[T] == PrologDictionary.EMPTY
                }
            }

            "discrepant values" {
                val a = Atom("a")
                val b = Atom("b")
                val T = Variable("T")
                val dA = PrologDictionary(mapOf(a to a), T)
                val dB = PrologDictionary(mapOf(a to b))

                dA shouldNotUnifyWith dB
            }
        }

        "rhs with fewer keys, values match for common keys" {
            val a = Atom("a")
            val b = Atom("b")
            val T = Variable("T")
            val dA = PrologDictionary(mapOf(a to a, b to b), T)
            val dB = PrologDictionary(mapOf(a to a))

            dA shouldNotUnifyWith dB
        }
    }

    // the same as "only LHS with tail", but inverted
    "only RHS with tail" - {
        "lhs with extra keys" - {
            "values for common keys match" {
                val a = Atom("a")
                val b = Atom("b")
                val T = Variable("T")
                val dA = PrologDictionary(mapOf(a to a, b to b))
                val dB = PrologDictionary(mapOf(a to a), T)

                dA shouldUnifyWith dB suchThat {
                    it.variableValues[T] == PrologDictionary(mapOf(b to b))
                }
            }

            "values for common keys discrepant" {
                val a = Atom("a")
                val b = Atom("b")
                val T = Variable("T")
                val dA = PrologDictionary(mapOf(a to b, b to b))
                val dB = PrologDictionary(mapOf(a to a), T)

                dA shouldNotUnifyWith dB
            }
        }

        "lhs with same keys" - {
            "matching values" {
                val a = Atom("a")
                val T = Variable("T")
                val dA = PrologDictionary(mapOf(a to a))
                val dB = PrologDictionary(mapOf(a to a), T)

                dA shouldUnifyWith dB suchThat {
                    it.variableValues[T] == PrologDictionary.EMPTY
                }
            }

            "discrepant values" {
                val a = Atom("a")
                val b = Atom("b")
                val T = Variable("T")
                val dA = PrologDictionary(mapOf(a to b))
                val dB = PrologDictionary(mapOf(a to a), T)

                dA shouldNotUnifyWith dB
            }
        }

        "lhs with fewer keys, values match for common keys" {
            val a = Atom("a")
            val b = Atom("b")
            val T = Variable("T")
            val dA = PrologDictionary(mapOf(a to a))
            val dB = PrologDictionary(mapOf(a to a, b to b), T)

            dA shouldNotUnifyWith dB
        }
    }

    "both with tail" - {
        "only common keys" - {
            "values match" {
                val a = Atom("a")
                val TA = Variable("TA")
                val TB = Variable("TB")
                val dA = PrologDictionary(mapOf(a to a), TA)
                val dB = PrologDictionary(mapOf(a to a), TB)

                dA shouldUnifyWith dB suchThat {
                    it.variableValues[TA] == PrologDictionary.EMPTY
                    &&
                    it.variableValues[TB] == PrologDictionary.EMPTY
                }
            }

            "values discrepant" {
                val a = Atom("a")
                val b = Atom("b")
                val TA = Variable("TA")
                val TB = Variable("TB")
                val dA = PrologDictionary(mapOf(a to a), TA)
                val dB = PrologDictionary(mapOf(a to b), TB)

                dA shouldNotUnifyWith dB
            }
        }

        "LHS with extra keys" - {
            "RHS only with common keys" - {
                "values match" {
                    val a = Atom("a")
                    val b = Atom("b")
                    val TA = Variable("TA")
                    val TB = Variable("TB")
                    val dA = PrologDictionary(mapOf(a to a, b to b), TA)
                    val dB = PrologDictionary(mapOf(a to a), TB)

                    dA shouldUnifyWith dB suchThat {
                        it.variableValues[TB] == PrologDictionary(mapOf(b to b))
                        &&
                        it.variableValues[TA] == PrologDictionary.EMPTY
                    }
                }

                "values discrepant" {
                    val a = Atom("a")
                    val b = Atom("b")
                    val TA = Variable("TA")
                    val TB = Variable("TB")
                    val dA = PrologDictionary(mapOf(a to a, b to b), TA)
                    val dB = PrologDictionary(mapOf(a to b), TB)

                    dA shouldNotUnifyWith dB
                }
            }

            "RHS with extra keys" - {
                "values for common keys match" {
                    val a = Atom("a")
                    val b = Atom("b")
                    val c = Atom("c")
                    val TA = Variable("TA")
                    val TB = Variable("TB")
                    val dA = PrologDictionary(mapOf(a to a, b to b), TA)
                    val dB = PrologDictionary(mapOf(a to a, c to c), TB)

                    dA shouldUnifyWith dB suchThat {
                        it.variableValues[TA] == PrologDictionary(mapOf(c to c))
                        &&
                        it.variableValues[TB] == PrologDictionary(mapOf(b to b))
                    }
                }

                "values for common keys discrepant" {
                    val a = Atom("a")
                    val b = Atom("b")
                    val c = Atom("c")
                    val TA = Variable("TA")
                    val TB = Variable("TB")
                    val dA = PrologDictionary(mapOf(a to a, b to b), TA)
                    val dB = PrologDictionary(mapOf(a to b, c to c), TB)

                    dA shouldNotUnifyWith dB
                }
            }
        }
    }
}}
