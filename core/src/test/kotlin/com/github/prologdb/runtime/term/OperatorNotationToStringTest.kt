package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.OperatorDefinition
import com.github.prologdb.runtime.knowledge.library.OperatorType
import io.kotlintest.matchers.shouldBe
import io.kotlintest.specs.FreeSpec

class OperatorNotationToStringTest : FreeSpec({
    "prefix operator nesting" - {
        val a = Atom("a")
        val b = Atom("b")
        val prefix = PredicateBuilder("prefix")
        val postfix = PredicateBuilder("postfix")
        val infix = PredicateBuilder("infix")
        
        "prefix has higher priority" - {
            "nesting XF" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(300, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))
                
                val term = prefix(postfix(a))
                
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a postfix"
            }
    
            "nesting YF" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(300, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.YF, "postfix"))
    
                val term = prefix(postfix(a))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a postfix"
            }
    
            "nesting XFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(300, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFX, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a infix b"
            }
    
            "nesting XFY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(300, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFY, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a infix b"
            }
    
            "nesting YFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(300, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.YFX, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a infix b"
            }
        }
        
        "prefix has lower priority" - {
            "nesting XF" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(100, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))
    
                val term = prefix(postfix(a))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix(a postfix)"
            }
    
            "nesting YF" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(100, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.YF, "postfix"))
    
                val term = prefix(postfix(a))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix(a postfix)"
            }
    
            "nesting XFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(100, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFX, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix(a infix b)"
            }
    
            "nesting XFY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(100, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFY, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix(a infix b)"
            }
    
            "nesting YFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(100, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.YFX, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix(a infix b)"
            }
        }
        
        "equal priority" - {
            "FX nesting XF" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))
    
                val term = prefix(postfix(a))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix(a postfix)"
            }
    
            "FX nesting YF" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.YF, "postfix"))
    
                val term = prefix(postfix(a))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix(a postfix)"
            }
    
            "FX nesting XFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFX, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix(a infix b)"
            }
    
            "FX nesting XFY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFY, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix(a infix b)"
            }
    
            "FX nesting YFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.YFX, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix(a infix b)"
            }
            
            "FY nesting XF" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FY, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))
    
                val term = prefix(postfix(a))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a postfix"
            }
    
            "FY nesting XFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FY, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFX, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a infix b"
            }
    
            "FY nesting XFY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FY, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFY, "infix"))
    
                val term = prefix(infix(a, b))
    
                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a infix b"
            }
        }
    }

    "postfix operator nesting" - {
        val a = Atom("a")
        val b = Atom("b")
        val prefix = PredicateBuilder("prefix")
        val postfix = PredicateBuilder("postfix")
        val infix = PredicateBuilder("infix")

        "postfix has higher priority" - {
            "nesting FX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(300, OperatorType.XF, "postfix"))

                val term = postfix(prefix(a))

                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a postfix"
            }

            "nesting FY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FY, "prefix"))
                registry.defineOperator(OperatorDefinition(300, OperatorType.XF, "postfix"))

                val term = postfix(prefix(a))

                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a postfix"
            }

            "nesting XFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(300, OperatorType.XF, "postfix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFX, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "a infix b postfix"
            }

            "nesting XFY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(300, OperatorType.XF, "postfix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFY, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "a infix b postfix"
            }

            "nesting YFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(300, OperatorType.XF, "postfix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.YFX, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "a infix b postfix"
            }
        }

        "postfix has lower priority" - {
            "nesting FX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(100, OperatorType.XF, "postfix"))

                val term = postfix(prefix(a))

                term.toStringUsingOperatorNotations(registry) shouldBe "(prefix a) postfix"
            }

            "nesting FY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FY, "prefix"))
                registry.defineOperator(OperatorDefinition(100, OperatorType.XF, "postfix"))

                val term = postfix(prefix(a))

                term.toStringUsingOperatorNotations(registry) shouldBe "(prefix a) postfix"
            }

            "nesting XFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))
                registry.defineOperator(OperatorDefinition(300, OperatorType.XFX, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "(a infix b) postfix"
            }

            "nesting XFY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))
                registry.defineOperator(OperatorDefinition(300, OperatorType.XFY, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "(a infix b) postfix"
            }

            "nesting YFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))
                registry.defineOperator(OperatorDefinition(300, OperatorType.YFX, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "(a infix b) postfix"
            }
        }

        "equal priority" - {
            "XF nesting FX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))

                val term = postfix(prefix(a))

                term.toStringUsingOperatorNotations(registry) shouldBe "(prefix a) postfix"
            }

            "XF nesting FY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FY, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))

                val term = postfix(prefix(a))

                term.toStringUsingOperatorNotations(registry) shouldBe "(prefix a) postfix"
            }

            "XF nesting XFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFX, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "(a infix b) postfix"
            }

            "XF nesting XFY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFY, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "(a infix b) postfix"
            }

            "XF nesting YFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.YFX, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "(a infix b) postfix"
            }

            "YF nesting XF" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.YF, "postfix"))

                val term = postfix(prefix(a))

                term.toStringUsingOperatorNotations(registry) shouldBe "prefix a postfix"
            }

            "YF nesting XFX" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.YF, "postfix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFX, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "a infix b postfix"
            }

            "YF nesting XFY" {
                val registry = DefaultOperatorRegistry()
                registry.defineOperator(OperatorDefinition(200, OperatorType.YF, "postfix"))
                registry.defineOperator(OperatorDefinition(200, OperatorType.XFY, "infix"))

                val term = postfix(infix(a, b))

                term.toStringUsingOperatorNotations(registry) shouldBe "(a infix b) postfix"
            }
        }
    }
    
    "infix nesting" - {
        val a = Atom("a")
        val b = Atom("b")
        val c = Atom("c")
        val lhsPrefix = PredicateBuilder("lhsPrefix")
        val rhsPostfix = PredicateBuilder("rhsPostfix")
        val infix = PredicateBuilder("infix")
        
        "lhs lower priority, rhs lower priority" {
            val registry = DefaultOperatorRegistry()
            registry.defineOperator(OperatorDefinition(300, OperatorType.XFX, "infix"))
            registry.defineOperator(OperatorDefinition(200, OperatorType.FY, "lhsPrefix"))
            registry.defineOperator(OperatorDefinition(200, OperatorType.YF, "rhsPostfix"))
            
            val term = infix(lhsPrefix(a), rhsPostfix(b))
            
            term.toStringUsingOperatorNotations(registry) shouldBe "lhsPrefix a infix b rhsPostfix"
        }

        "lhs lower priority, rhs higher priority" {
            val registry = DefaultOperatorRegistry()
            registry.defineOperator(OperatorDefinition(300, OperatorType.XFX, "infix"))
            registry.defineOperator(OperatorDefinition(200, OperatorType.FY, "lhsPrefix"))
            registry.defineOperator(OperatorDefinition(400, OperatorType.YF, "rhsPostfix"))

            val term = infix(lhsPrefix(a), rhsPostfix(b))

            term.toStringUsingOperatorNotations(registry) shouldBe "lhsPrefix a infix (b rhsPostfix)"
        }

        "lhs higher priority, rhs lower priority" {
            val registry = DefaultOperatorRegistry()
            registry.defineOperator(OperatorDefinition(300, OperatorType.XFX, "infix"))
            registry.defineOperator(OperatorDefinition(400, OperatorType.FY, "lhsPrefix"))
            registry.defineOperator(OperatorDefinition(200, OperatorType.YF, "rhsPostfix"))

            val term = infix(lhsPrefix(a), rhsPostfix(b))

            term.toStringUsingOperatorNotations(registry) shouldBe "(lhsPrefix a) infix b rhsPostfix"
        }

        "lhs higher priority, rhs higher priority" {
            val registry = DefaultOperatorRegistry()
            registry.defineOperator(OperatorDefinition(300, OperatorType.XFX, "infix"))
            registry.defineOperator(OperatorDefinition(400, OperatorType.FY, "lhsPrefix"))
            registry.defineOperator(OperatorDefinition(400, OperatorType.YF, "rhsPostfix"))

            val term = infix(lhsPrefix(a), rhsPostfix(b))

            term.toStringUsingOperatorNotations(registry) shouldBe "(lhsPrefix a) infix (b rhsPostfix)"
        }
    }
})