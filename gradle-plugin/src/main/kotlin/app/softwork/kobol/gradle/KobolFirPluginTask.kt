package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.tasks.*
import org.gradle.api.tasks.PathSensitivity.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class KobolFirPluginTask : DefaultTask() {
    init {
        group = "kobol"
    }

    @get:Internal
    internal val pluginConfiguration = project.configurations.dependencyScope("${name}Plugin") {
        dependencies.add(project.dependencies.create("app.softwork.kobol:intellij-env:$KOBOL_VERSION"))
    }.name

    @get:InputFiles
    @get:Classpath
    internal val pluginClasspath = project.configurations.resolvable("${name}Classpath")

    public fun plugin(dependency: Any) {
        project.dependencies.add(pluginConfiguration, dependency)
    }

    @get:InputFiles
    @get:PathSensitive(RELATIVE)
    public abstract val sources: ConfigurableFileCollection

    @get:OutputDirectory
    public abstract val outputFolder: DirectoryProperty

    init {
        outputFolder.convention(project.layout.buildDirectory.dir("reports/kobol/plugins"))
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generateFlow() {
        workerExecutor.classLoaderIsolation {
            classpath.from(pluginClasspath)
        }.submit(FirKobolAction::class.java) {
            inputFiles.setFrom(sources)
            outputFolder.set(this@KobolFirPluginTask.outputFolder)
        }
    }
}
