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

1\. Declare module source:

```kotlin
val moduleSourceCode = """
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
// this allows the runtime to load our custom source code
val userModuleReference = ModuleReference("prog", "user")
val userModuleLoader = PredefinedSourceModuleLoader().apply {
  registerModule(userModuleReference, moduleSourceCode)
}
```

Make sure to import all modules you need; have a look at the playground.

2\. Initialize a new Runtime:

```kotlin
// this adds the standard library to the runtime
val moduleLoader = CascadingModuleLoader(userModuleLoader, StandardLibraryModuleLoader)

// this initializes the runtime
val runtime = DefaultPrologRuntimeEnvironment(moduleLoader)
val loadedUserModule = runtime.assureModuleLoaded(userModuleReference)
```

You can load module source code from the classpath with `ClasspathPrologSourceModuleLoader`,
and Kotlin-only modules with `PredefinedModuleLoader`. For a combination, look at [how the
[standard library does it](stdlib/src/main/kotlin/com/github/prologdb/runtime/stdlib/loader/StandardLibraryModuleLoader.kt)

3\. Run a query


```kotlin
val queryCode = """jealous(marcellus, Person)."""
val queryLexer = Lexer(
  SourceUnit("query"),
  LineEndingNormalizer(CharacterIterable(queryCode).iterator())
)
val queryParseResult = PrologParser().parseQuery(queryLexer, loadedUserModule.localOperators)
if (queryParseResult.reportings.isNotEmpty()) {
  // parsing not successful
}
val queryAST = queryParseResult.item!!
```

Once you have the query AST, you can run it:

```kotlin
val solutions: LazySequence<Unification> = runtime.fulfill(userModuleReference.moduleName, queryAST)
val personsJealousOfMarcellus = solutions
  .mapRemaining { it.variableValues[Variable("Person")] }
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