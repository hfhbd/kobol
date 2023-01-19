package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.Named
import org.gradle.api.artifacts.*
import org.gradle.api.file.*
import org.gradle.api.tasks.*
import javax.inject.*

public abstract class CobolSource @Inject constructor(
    name: String,
    file: FileSystemLocation,
    private val project: Project
) : Named, FileSystemLocation by file {
    private val name = name.lowercase()
    private val nameTitle = this.name.replaceFirstChar {
        it.titlecaseChar()
    }

    override fun getName(): String = name

    public val plugins: NamedDomainObjectProvider<Configuration> =
        project.configurations.register("kobol${nameTitle}Plugin") {
            isCanBeResolved = true
            isCanBeConsumed = false
        }

    public fun plugin(dependency: Any) {
        project.dependencies.add(plugins.name, dependency)
    }

    public val convert: TaskProvider<KobolTask> =
        project.tasks.register("convert${nameTitle}Cobol", KobolTask::class.java) {
            classpath.from(plugins)
            sources.from(file)
        }
}
