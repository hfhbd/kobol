// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
// https://github.com/JetBrains/gradle-grammar-kit-plugin
// Changes by hfhbd: Removed unused util functions

package org.jetbrains.grammarkit

import org.gradle.api.file.FileSystemLocation
import org.gradle.api.provider.Provider

internal const val GROUP_NAME = "grammarKit"

internal val <T : FileSystemLocation> Provider<T>.path
    get() = get().asFile.canonicalPath
