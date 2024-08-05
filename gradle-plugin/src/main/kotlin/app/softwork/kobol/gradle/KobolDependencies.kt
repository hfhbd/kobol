package app.softwork.kobol.gradle

import org.gradle.api.artifacts.dsl.Dependencies
import org.gradle.api.artifacts.dsl.DependencyCollector

interface KobolDependencies : Dependencies {
    val compiler: DependencyCollector
}
