package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.Named
import org.gradle.api.artifacts.*
import org.gradle.api.artifacts.dsl.*
import org.gradle.api.file.*
import javax.inject.*

public abstract class CobolSource @Inject constructor(
    name: String,
    file: FileSystemLocation,
    project: Project
) : Named, FileSystemLocation by file {
    private val name = name.lowercase()
    private val nameTitle = this.name.replaceFirstChar {
        it.titlecaseChar()
    }
    public val taskName: String = "convert${nameTitle}Cobol"

    override fun getName(): String = name

    public val plugins: NamedDomainObjectProvider<Configuration> = project.configurations.register("kobol${nameTitle}Plugin") {
        isCanBeResolved = true
        isCanBeConsumed = false
        isVisible = false
    }
    
    public fun DependencyHandler.plugin(dependency: Any): Dependency? {
        return add(plugins.name, dependency)
    }
}

public fun DependencyHandler.kobolPlugin(source: NamedDomainObjectProvider<CobolSource>, dependency: Any) {
    source.configure {
        plugin(dependency)
    }
}
