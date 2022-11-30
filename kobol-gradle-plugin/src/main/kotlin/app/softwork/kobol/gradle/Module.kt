package app.softwork.kobol.gradle

import org.gradle.api.artifacts.dsl.DependencyHandler

public fun DependencyHandler.kobol(module: String): String = "app.softwork:kobol-$module:$kobolVersion"
