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
    internal val pluginConfiguration = project.configurations.register("${name}Plugin") {
        it.isVisible = false
        it.isCanBeConsumed = false
        it.isCanBeResolved = true

        it.dependencies.add(project.dependencies.create("app.softwork.kobol:intellij-env:$KOBOL_VERSION"))
    }.name

    public fun plugin(dependency: Any) {
        project.dependencies.add(pluginConfiguration, dependency)
    }

    @get:InputFiles
    @get:PathSensitive(RELATIVE)
    public abstract val sources: ConfigurableFileCollection

    @get:InputFiles
    @get:Classpath
    internal val pluginDependencies =
        project.objects.fileCollection().from(project.configurations.named(pluginConfiguration))

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
            it.classpath.from(pluginDependencies)
        }.submit(FirKobolAction::class.java) {
            it.inputFiles.setFrom(sources)
            it.outputFolder.set(outputFolder)
        }
    }
}
