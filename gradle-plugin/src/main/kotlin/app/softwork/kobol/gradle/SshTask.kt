package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.kotlin.dsl.dependencies
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

    @get:InputFiles
    @get:Classpath
    internal val sshClasspath: ConfigurableFileCollection = project.objects.fileCollection()
    init {
        val configuration = project.configurations.dependencyScope("${name}Ssh")
        project.dependencies {
            configuration("app.softwork.kobol:ssh-env:$KOBOL_VERSION")
        }
        sshClasspath.from(
            project.configurations.resolvable("${name}SshClasspath") {
                extendsFrom(configuration.get())
            },
        )
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor
}
