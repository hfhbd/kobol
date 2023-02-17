package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.artifacts.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.provider.Provider
import org.gradle.api.tasks.*
import org.gradle.api.tasks.PathSensitivity.*
import org.gradle.work.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class KobolFirPluginTask : DefaultTask() {
    init {
        group = "Kobol"
    }
    
    public val pluginConfiguration: String = project.configurations.register("${name}Plugin") {
        isVisible = false
        isCanBeConsumed = false
        isCanBeResolved = true
    }.name

    private val pluginDependencies = project.configurations.named(pluginConfiguration).map { it.fileCollection() }
    
    public fun firPlugin(dependency: Any) {
        project.dependencies.add(pluginConfiguration, dependency)
    }
    
    public fun add(cobolSource: Provider<CobolSource>) {
        sources.from(cobolSource.map { it.file })
        plugins.from(cobolSource.flatMap { project.configurations.named(it.plugins) })
    }

    @get:InputFiles
    @get:PathSensitive(RELATIVE)
    public abstract val sources: ConfigurableFileCollection

    @get:InputFiles
    @get:PathSensitive(RELATIVE)
    public abstract val plugins: ConfigurableFileCollection

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
            classpath.from(pluginDependencies, plugins)
        }.submit(FirKobolAction::class.java) {
            inputFiles.setFrom(sources)
            outputFolder.set(this@KobolFirPluginTask.outputFolder)
        }
    }
}
