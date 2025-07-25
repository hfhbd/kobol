package app.softwork.kobol.gradle

import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.internal.plugins.software.SoftwareType
import org.gradle.api.tasks.SourceSetContainer
import javax.inject.Inject

public abstract class KobolPlugin : Plugin<Project> {
    @get:SoftwareType(name = "kobol")
    public abstract val kobol: Kobol

    @get:Inject
    public abstract val sourceSetContainer: SourceSetContainer

    override fun apply(target: Project) {
        target.configureExtension(kobol, sourceSetContainer)
    }
}
