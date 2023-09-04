// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
// https://github.com/JetBrains/gradle-grammar-kit-plugin
// Changes by hfhbd: inline constants

package org.jetbrains.grammarkit

import org.gradle.api.provider.Property

abstract class GrammarKitPluginExtension {

    /**
     * The release version of the [Grammar-Kit](https://github.com/JetBrains/Grammar-Kit) to use.
     */
    abstract val grammarKitRelease: Property<String>

    /**
     * The version of the IntelliJ-patched JFlex, a [fork of JFlex](https://github.com/JetBrains/intellij-deps-jflex)
     * lexer generator for IntelliJ Platform API.
     */
    abstract val jflexRelease: Property<String>

    /**
     * Version of the IntelliJ to build the classpath for [org.jetbrains.grammarkit.tasks.GenerateParserTask]
     * and [org.jetbrains.grammarkit.tasks.GenerateLexerTask] tasks.
     * If provided, [grammarKitRelease] and [jflexRelease] properties are ignored as both dependencies will be provided
     * from the given IntelliJ IDEA release.
     */
    abstract val intellijRelease: Property<String>

    init {
        grammarKitRelease.convention("2022.3.1")
        jflexRelease.convention("1.9.1")
    }
}
