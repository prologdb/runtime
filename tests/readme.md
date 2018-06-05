This project is supposed to provide tests for the runtime written
in prolog. The goals are

* testthe basic functionality of the system
* test all builtins
* serve as a measure of performance

----

## Prolog Test cases 

The test files must be placed in `src/test/prolog` and must have
the extension `.test.pl`. They can contain arbitrary prolog code.
Code in one test file cannot be accessed from another test file.

The test cases are defined using a special syntax:

    test "string_chars/2 conversion from string to list" by [
        string_chars("foobar", X),
        X = [f, o, o, b, a, r]
    ].
    
The syntax uses these operator definitions:

    :- op(100,fx,test).
    :- op(800,xfx,by).
    
Thus, the pretty syntax `display/1`s as:

    by(
        test("string_chars/2 conversion from string to list"),
        [
            string_chars("foobar", X),
            X = [f, o, o, b, a, r]
        ]
    ).

The string argument to test/1 describes the test. The second
argument to requires/2 is a list of goals. When running the
test, the goals will be run against a knowledge base constructed
from the other declarations in the file. If a goal fails, the
test will be considered a failure.

Note:

* Changes are reverted between tests.
* The order of execution may vary between test runs