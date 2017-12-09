# Prolog Runtime written in Kotlin

This is a prolog runtime written in Kotlin. The actual runtime
is in the `core` submodule; the `core-js` and `core-jvm` compile
the sources from the `core` submodule to JavaScript and JVM classes,
respectively.

The `parser` submodule contains code to parse prolog source. Because
this currently depends on some Java IO classes it cannot compile to
JS, hence there is no `parser-js` module.

---

To use this like a regular prolog system, run the `jvm-playground`.

# Installation

To use this within other projects, use the maven central repository:

```xml
<!-- Java / JVM projects -->
<dependency>
    <groupId>com.github.prologdb</groupId>
    <artifactId>runtime-core-jvm</groupId>
    <version>1.0.0-RC1</version>
</dependency>
<dependency>
    <groupId>com.github.prologdb</groupId>
    <artifactId>runtime-parser-jvm</groupId>
    <version>1.0.0-RC1</version>
</dependency>

<!-- JS projects -->
<dependency>
    <groupId>com.github.prologdb</groupId>
    <artifactId>runtime-core-js</groupId>
    <version>1.0.0-RC1</version>
</dependency>
```