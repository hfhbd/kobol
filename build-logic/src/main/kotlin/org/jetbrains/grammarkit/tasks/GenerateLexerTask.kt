// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
// https://github.com/JetBrains/gradle-grammar-kit-plugin
// Changes by hfhbd: Remove useless attributes exec function

package org.jetbrains.grammarkit.tasks

import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.jetbrains.grammarkit.GROUP_NAME
import org.jetbrains.grammarkit.path

/**
 * The `generateLexer` task generates a lexer for the given grammar.
 * The task is configured using common [org.jetbrains.grammarkit.GrammarKitPluginExtension] extension.
 */
@CacheableTask
abstract class GenerateLexerTask : JavaExec() {

    init {
        group = GROUP_NAME

        mainClass.set("jflex.Main")

        argumentProviders.add {
            listOf(
                "-d",
                targetOutputDir.path.get(),
                sourceFile.path.get()
            )
        }
    }

    /**
     * Required.
     * The output directory for the generated lexer.
     */
    @get:OutputDirectory
    abstract val targetOutputDir: DirectoryProperty

    /**
     * Required.
     * The source Flex file to generate the lexer from.
     */
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val sourceFile: RegularFileProperty
}
