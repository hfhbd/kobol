package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.Provider
import org.gradle.api.tasks.*
import org.gradle.api.tasks.PathSensitivity.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class KobolFirPluginTask : DefaultTask() {
    init {
        group = "Kobol"
    }

    @get:Internal
    public val pluginConfiguration: String = project.configurations.register("${name}Plugin") {
        it.isVisible = false
        it.isCanBeConsumed = false
        it.isCanBeResolved = true
    }.name

    @get:InputFiles
    @get:Classpath
    internal val pluginDependencies =
        project.objects.fileCollection().from(project.configurations.named(pluginConfiguration))

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
    @get:Classpath
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
            it.classpath.from(pluginDependencies, plugins)
        }.submit(FirKobolAction::class.java) {
            it.inputFiles.setFrom(sources)
            it.outputFolder.set(outputFolder)
        }
    }
}
