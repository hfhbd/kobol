# kobol

An experimental COBOL to JVM based language (Kotlin or Java) transpiler with DB2-SQL support.

## Setup with Gradle

```kotlin
plugins {
    id("java-library")
    id("app.softwork.kobol")
}

kobol {
    dependencies {
        compiler(kotlin()) // for Kotlin output files
        compiler(kotlinFileJava()) // to use java.io.File in Kotlin

        compiler(kotlinSqldelight()) // to use sqldelight for SQL code

        compiler(pluginOptimize()) // refactors the code to be more object orientated

        compiler("app.cash.sql-psi:environment:0.5.2") // current workaround for https://github.com/hfhbd/kobol/issues/879
        compiler("app.cash.sqldelight:compiler-env:2.1.0") // current workaround for https://github.com/hfhbd/kobol/issues/879
    }
}
```

The cobol code files need to be stored in `src/main/cobol` with `.cbl` ending.
The resulting code will be generated in `build/generated/kobol/main`.

## Optional IntelliJ plugin

To install the optional IntelliJ plugin to get code highlighting, setup a custom plugin repository with this url: 
`https://hfhbd.github.io/kobol/updatePlugins.xml`.
You might need to search for Kobol in the custom repository explicitly. 
