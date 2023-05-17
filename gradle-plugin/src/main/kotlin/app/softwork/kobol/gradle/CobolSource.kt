package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.Named
import org.gradle.api.artifacts.*
import org.gradle.api.artifacts.dsl.*
import org.gradle.api.file.*
import org.gradle.api.provider.Provider
import org.gradle.api.tasks.*
import org.gradle.kotlin.dsl.*
import javax.inject.*

public abstract class CobolSource @Inject constructor(
    name: String,
    public val file: FileSystemLocation,
    project: Project
) : Named {
    private val name = name.lowercase()
    public val taskName: String = this.name.taskName()

    override fun getName(): String = name

    public val plugins: String = project.configurations.register(name.pluginName()) {
        dependencies.add(project.dependencies.create("app.softwork.kobol:ir:$KOBOL_VERSION"))
        dependencies.add(project.dependencies.create("app.softwork.kobol:intellij-env:$KOBOL_VERSION"))
        isCanBeResolved = true
        isCanBeConsumed = false
        isVisible = false
    }.name

    public fun DependencyHandler.plugin(dependency: Any): Dependency? = add(plugins, dependency)

    public val TaskContainer.convert: TaskProvider<out KobolTask> get() = named<KobolTask>(taskName)
    public fun TaskContainer.convert(configuration: Action<KobolTask>) {
        named<KobolTask>(taskName) { 
            configuration(this) 
        }
    }
}

private fun String.nameTitle() = replaceFirstChar {
    it.titlecaseChar()
}

private fun String.taskName() = "convert${nameTitle()}Cobol"
private fun String.pluginName() = "kobol${nameTitle()}Plugin"

public fun DependencyHandler.kobolPlugin(source: NamedDomainObjectProvider<CobolSource>, dependency: Any): Dependency? =
    add(source.name.pluginName(), dependency)

public fun TaskContainer.convert(source: NamedDomainObjectProvider<CobolSource>): TaskProvider<out KobolTask> {
    return named<KobolTask>(source.name.taskName())
}

public fun TaskContainer.convert(source: NamedDomainObjectProvider<CobolSource>, configuration: Action<KobolTask>) {
    named<KobolTask>(source.name.taskName()).configure(configuration)
}

public val NamedDomainObjectProvider<CobolSource>.file: Provider<FileSystemLocation> get() = map { it.file }
