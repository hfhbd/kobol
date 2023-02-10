package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.Named
import org.gradle.api.artifacts.*
import org.gradle.api.artifacts.dsl.*
import org.gradle.api.file.*
import org.gradle.api.tasks.*
import org.gradle.kotlin.dsl.*
import javax.inject.*

public abstract class CobolSource @Inject constructor(
    name: String,
    public val file: FileSystemLocation,
    project: Project
) : Named {
    private val name = name.lowercase()
    private val nameTitle = this.name.nameTitle()
    public val taskName: String = this.name.taskName()
    
    override fun getName(): String = name

    public val plugins: String = project.configurations.register("kobol${nameTitle}Plugin") {
        isCanBeResolved = true
        isCanBeConsumed = false
        isVisible = false
    }.name
    
    public fun DependencyHandler.plugin(dependency: Any): Dependency? {
        return add(plugins, dependency)
    }

    public fun TaskContainer.convert(): TaskProvider<KobolTask> = named<KobolTask>(taskName)
    public fun TaskContainer.convert(configuration: Action<KobolTask>) {
        named<KobolTask>(taskName).configure(configuration)
    }
}

private fun String.nameTitle() = replaceFirstChar {
    it.titlecaseChar()
}

private fun String.taskName() = "convert${nameTitle()}Cobol"


public fun DependencyHandler.kobolPlugin(source: NamedDomainObjectProvider<CobolSource>, dependency: Any) {
    source.configure {
        plugin(dependency)
    }
}

public fun TaskContainer.convert(source: NamedDomainObjectProvider<CobolSource>): TaskProvider<KobolTask> {
    return named<KobolTask>(source.name.taskName())
}

public fun TaskContainer.convert(source: NamedDomainObjectProvider<CobolSource>, configuration: Action<KobolTask>) {
    return named<KobolTask>(source.name.taskName()).configure(configuration)
}
