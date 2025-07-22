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

    @get:Classpath
    internal val pluginClasspath: ConfigurableFileCollection = project.objects.fileCollection()

    @get:InputFiles
    @get:SkipWhenEmpty
    @get:IgnoreEmptyDirectories
    @get:PathSensitive(RELATIVE)
    public abstract val sources: ConfigurableFileCollection

    @get:OutputDirectory
    public abstract val outputFolder: DirectoryProperty

    init {
        outputFolder.convention(project.layout.buildDirectory.dir("reports/kobol/plugins/$name"))
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generateFlow() {
        workerExecutor.classLoaderIsolation {
            it.classpath.from(pluginClasspath)
        }.submit(FirKobolAction::class.java) {
            it.inputFiles.setFrom(sources)
            it.outputFolder.set(this@KobolFirPluginTask.outputFolder)
        }
    }
}
