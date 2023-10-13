package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.artifacts.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.work.*
import org.gradle.workers.*
import javax.inject.*

@DisableCachingByDefault
public abstract class SshTask : DefaultTask() {
    init {
        group = "kobol"
        folder.convention(project.name)
    }

    @get:Input
    public abstract val host: Property<String>

    @get:Input
    public abstract val user: Property<String>

    @get:Input
    public abstract val folder: Property<String>

    @get:Internal
    public val configuration: NamedDomainObjectProvider<DependencyScopeConfiguration> =
        project.configurations.dependencyScope("${name}Ssh") {
            dependencies.add(project.dependencies.create("app.softwork.kobol:ssh-env:$KOBOL_VERSION"))
        }

    @get:InputFiles
    @get:Classpath
    internal val sshClasspath: NamedDomainObjectProvider<ResolvableConfiguration> =
        project.configurations.resolvable("${name}SshClasspath") {
            extendsFrom(configuration.get())
        }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor
}
