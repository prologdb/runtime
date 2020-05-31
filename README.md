# Prolog Runtime written in Kotlin

This is a prolog runtime written in Kotlin. It utilizes Kotlins subroutines
to model the search tree; as such it has a strong focus on asynchronicity.

You can start a visual prolog prompt by running the build artifact of `jvm-playground`:

```bash
cd jvm-playground
mvn clean package
java -jar target/runtime-jvm-playground-*.jar
```

# Usage

To use this interpreter in other projects, import it via Maven Central:

```xml
<dependency>
     <groupId>com.github.prologdb</groupId>
     <artifactId>runtime</artifactId>
     <version><!-- see latest version --></version>
</dependency>
```

You can find the following working code in [ReadmeExample.kt](jvm-playground/src/main/kotlin/ReadmeExample.kt).

1\. Parse module sources:

```kotlin
val moduleSourceCode: String = """
:- use_module(library(equality)).
loves(vincent, mia).
loves(marcellus, mia).
loves(pumpkin, honey_bunny).
loves(honey_bunny, pumpkin).

jealous(X, Y) :-
    loves(X, Z),
    loves(Y, Z),
    X \= Y
    .
"""
val lexer = Lexer(
    SourceUnit("root module"),
    LineEndingNormalizer(
        CharacterIterable(moduleSourceCode).iterator()
    )
)

val moduleParseResult = PrologParser().parseModule(lexer, ISOOpsOperatorRegistry, ModuleDeclaration("_root", null))
if (moduleParseResult.reportings.isNotEmpty()) {
    // parsing not successful
}

val module = moduleParseResult.item!!
```

Make sure to import all modules you need; have a look at the playground.

2\. Initialize a new Runtime:

```kotlin
val runtime = PrologRuntimeEnvironment(module)
```

To add your own modules (loaded form source code or programmed in Kotlin):

```kotlin
class CustomModule : com.github.prologdb.runtime.module.Module {
    override val name = "custom"
    // ...
}
val runtime = PrologRuntimeEnvironment(module, NativeLibraryLoader.withCoreLibraries().apply {
    registerModule("myapp", CustomModule())
})
```

The custom module can then be loaded into other modules (including the root module)
via the `use_module` directive:
```prolog
:- use_module(myapp(custom)).
```

3\. Run a query

Parsing the query is similar to parsing the module. Be sure to pass on the
modules operator definitions to the parser:

```kotlin
val queryCode = """jealous(marcellus, Person)."""
val queryLexer = Lexer(
    SourceUnit("query"),
    LineEndingNormalizer(CharacterIterable(queryCode).iterator())
)
val queryParseResult = PrologParser().parseQuery(queryLexer, module.localOperators)
if (queryParseResult.reportings.isNotEmpty()) {
    // parsing not successful
}
val queryAST = queryParseResult.item!!
```

Once you have the query AST, you can run it:

```kotlin
val solutions: LazySequence<Unification> = runtime.fulfill(queryAST, ReadWriteAuthorization)
val personsJealousOfMarcellus: Set<String> = solutions
    .mapRemaining { it.variableValues[PrologVariable("Person")] }
    .mapRemaining { (it as Atom).name }
    .remainingTo(::mutableSetOf)
```

# Modules

* async: async and coroutine primitives that are suitable for the prolog proof search 
  semantics (`kotlin.sequences.Sequence` is not!). Has no ties to actual prolog.
* core
  * defines the structure of the core elements: terms, modules, queries
  * all semantics necessary to interpret ASTs (unification, proof search)
  * builtins: predicates (e.g. `=/2`, `append/3`, ...), operators, maths (`is/2`)
* parser: parses prolog source into an AST as defined in the core module
* jvm-playground: a simple visual prompt. Used mainly for testing during development.
* tests: end-to-end tests for all builtin functionality