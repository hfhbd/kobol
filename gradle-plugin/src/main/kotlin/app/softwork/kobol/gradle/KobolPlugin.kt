package app.softwork.kobol.gradle

import org.gradle.api.Plugin
import org.gradle.api.Project

public abstract class KobolPlugin : Plugin<Project> {
    override fun apply(target: Project) {
        target.pluginManager.apply("org.jetbrains.kotlin.jvm")
        val kobol = target.extensions.create<Kobol>("kobol", Kobol::class.java)
        target.configureExtension(kobol)
    }
}
