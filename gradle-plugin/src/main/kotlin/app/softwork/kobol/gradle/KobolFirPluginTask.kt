package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.tasks.*
import org.gradle.api.tasks.PathSensitivity.*
import org.gradle.kotlin.dsl.dependencies
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class KobolFirPluginTask : DefaultTask() {
    init {
        group = "kobol"
    }

    @get:Internal
    internal val pluginConfiguration: String

    @get:InputFiles
    @get:Classpath
    internal val pluginClasspath: ConfigurableFileCollection = project.objects.fileCollection()

    init {
        val pluginConfiguration = project.configurations.dependencyScope("${name}Plugin")
        this.pluginConfiguration = pluginConfiguration.name
        project.dependencies {
            pluginConfiguration("app.softwork.kobol:intellij-env:$KOBOL_VERSION")
        }
        pluginClasspath.from(
            project.configurations.resolvable("${name}Classpath") {
                extendsFrom(project.configurations.getByName(this@KobolFirPluginTask.pluginConfiguration))
            },
        )
    }

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
