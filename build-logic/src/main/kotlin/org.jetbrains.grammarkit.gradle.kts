// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
// https://github.com/JetBrains/gradle-grammar-kit-plugin
// Changes by hfhbd: Refactor plugin code to precompiled plugin

import gradle.kotlin.dsl.accessors._64de664e3f3ca753fb856e13fe0d41c2.compileClasspath
import gradle.kotlin.dsl.accessors._64de664e3f3ca753fb856e13fe0d41c2.compileOnly
import org.jetbrains.grammarkit.GROUP_NAME
import org.jetbrains.grammarkit.GrammarKitPluginExtension
import org.jetbrains.grammarkit.tasks.GenerateLexerTask
import org.jetbrains.grammarkit.tasks.GenerateParserTask
import java.io.File

plugins {
    id("java")
}

val extension = extensions.create<GrammarKitPluginExtension>(GROUP_NAME)

val grammarKitClassPath by configurations.registering

tasks.register<GenerateLexerTask>("generateLexer")

tasks.withType<GenerateLexerTask>().configureEach {
    classpath(getClasspath(grammarKitClassPath, configurations.compileClasspath) { file ->
        file.name.startsWith("jflex")
    })
}

tasks.register<GenerateParserTask>("generateParser")

tasks.withType<GenerateParserTask>().configureEach {
    val requiredLibs = listOf(
        "app", "jdom", "trove4j", "junit", "guava", "asm-all", "automaton", "platform-api", "platform-impl",
        "util", "util_rt", "annotations", "picocontainer", "extensions", "idea", "openapi", "grammar-kit",
        "platform-util-ui", "platform-concurrency", "intellij-deps-fastutil",
        // CLion unlike IDEA contains `MockProjectEx` in `testFramework.jar` instead of `idea.jar`
        // so this jar should be in `requiredLibs` list to avoid `NoClassDefFoundError` exception
        // while parser generation with CLion distribution
        "testFramework", "3rd-party",
    )

    classpath(getClasspath(grammarKitClassPath, configurations.compileClasspath) { file ->
        requiredLibs.any {
            file.name.equals("$it.jar", true) || file.name.startsWith("$it-", true)
        }
    })
}

configurations.compileOnly {
    val grammarJFlexDependencies =
        extension.grammarKitRelease.zip(
            extension.jflexRelease,
        ) { grammarKitRelease, jflexRelease ->
            listOf(
                "org.jetbrains:grammar-kit:$grammarKitRelease",
                "org.jetbrains.intellij.deps.jflex:jflex:$jflexRelease",
            ).map(project.dependencies::create).map {
                it as ModuleDependency
                it.exclude(mapOf("group" to "org.jetbrains.plugins", "module" to "ant"))
                it.exclude(mapOf("group" to "org.jetbrains.plugins", "module" to "idea"))
            }
        }

    dependencies.addAllLater(
        extension.intellijRelease.zip(grammarJFlexDependencies) { intellijRelease, dependencies ->
            when {
                intellijRelease.isEmpty() -> dependencies
                else -> emptyList()
            }
        })
}

grammarKitClassPath {
    val platformDependencies = zip(
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
        ).map(project.dependencies::create).map {
            it as ModuleDependency
            it.exclude(mapOf("group" to "com.jetbrains.rd"))
            it.exclude(mapOf("group" to "org.jetbrains.marketplace"))
            it.exclude(mapOf("group" to "org.roaringbitmap"))
            it.exclude(mapOf("group" to "org.jetbrains.plugins"))
            it.exclude(mapOf("module" to "idea"))
            it.exclude(mapOf("module" to "ant"))
        }
    }
    dependencies.addAllLater(
        extension.intellijRelease.zip(platformDependencies) { intellijRelease, dependencies ->
            when {
                intellijRelease.isEmpty() -> emptyList()
                else -> dependencies
            }
        })
}

fun getClasspath(
    grammarKitClassPathConfiguration: Provider<Configuration>,
    compileClasspathConfiguration: Provider<Configuration>,
    filter: (File) -> Boolean,
): Provider<Collection<File>> =
    grammarKitClassPathConfiguration.zip(compileClasspathConfiguration) { grammar, compile ->
        if (!grammar.isEmpty) {
            grammar.files
        } else compile.files.filter(filter)
    }
