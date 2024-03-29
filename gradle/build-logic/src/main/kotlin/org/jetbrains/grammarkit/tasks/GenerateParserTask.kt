// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
// https://github.com/JetBrains/gradle-grammar-kit-plugin
// Changes by hfhbd: Remove useless attributes exec function

package org.jetbrains.grammarkit.tasks

import org.gradle.api.file.*
import org.gradle.api.tasks.*
import org.jetbrains.grammarkit.*

/**
 * The `generateParser` task generates a parser for the given grammar.
 * The task is configured using common [org.jetbrains.grammarkit.GrammarKitPluginExtension] extension.
 */
@CacheableTask
abstract class GenerateParserTask : JavaExec() {
    /**
     * The source BNF file to generate the parser from.
     */
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val sourceFile: RegularFileProperty

    /**
     * The output root directory.
     */
    @get:OutputDirectory
    abstract val targetRootOutputDir: DirectoryProperty

    init {
        description = "Generates parsers for IntelliJ-based plugin"
        group = GROUP_NAME

        mainClass.set("org.intellij.grammar.Main")
        argumentProviders.add {
            listOf(
                targetRootOutputDir.path,
                sourceFile.path,
            )
        }
    }
}
