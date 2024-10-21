// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
// https://github.com/JetBrains/gradle-grammar-kit-plugin
// Changes by hfhbd: Refactor plugin code to precompiled plugin

import org.jetbrains.grammarkit.*
import org.jetbrains.grammarkit.tasks.*

plugins {
    id("java-library")
}

val extension = extensions.create<GrammarKitPluginExtension>(GROUP_NAME)

val grammarKit by configurations.dependencyScope("grammarKit") {
    exclude(group = "com.jetbrains.rd")
    exclude(group = "org.jetbrains.marketplace")
    exclude(group = "org.roaringbitmap")
    exclude(group = "org.jetbrains.plugins")
    exclude(module = "idea")
    exclude(module = "ant")

    dependencies.addAllLater(zip(
        extension.intellijRelease,
        extension.grammarKitRelease,
        extension.jflexRelease,
    ) { intellijRelease, grammarKitRelease, jflexRelease ->
        listOf(
            "org.jetbrains:grammar-kit:$grammarKitRelease",
            "org.jetbrains.intellij.deps.jflex:jflex:$jflexRelease",
            "com.jetbrains.intellij.platform:indexing-impl:$intellijRelease",
            "com.jetbrains.intellij.platform:analysis-impl:$intellijRelease",
            "com.jetbrains.intellij.platform:core-impl:$intellijRelease",
            "com.jetbrains.intellij.platform:lang-impl:$intellijRelease",
            "org.jetbrains.intellij.deps:asm-all:7.0.1",
        ).map(project.dependencies::create)
    })
}

val grammarKitClassPath = configurations.resolvable("grammarKitClassPath") {
    extendsFrom(grammarKit)
}

tasks.register<GenerateLexerTask>("generateLexer")

tasks.withType<GenerateLexerTask>().configureEach {
    classpath(grammarKitClassPath)
}

tasks.register<GenerateParserTask>("generateParser")

tasks.withType<GenerateParserTask>().configureEach {
    classpath(grammarKitClassPath)
}
