package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.tasks.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class KobolFlowGraph: DefaultTask() {
    init {
        group = "Kobol"
    }

    @get:InputFiles
    @get:SkipWhenEmpty
    @get:IgnoreEmptyDirectories
    @get:PathSensitive(PathSensitivity.RELATIVE)
    public abstract val sources: ConfigurableFileCollection

    @get:InputFiles
    @get:PathSensitive(PathSensitivity.RELATIVE)
    internal abstract val classpath: ConfigurableFileCollection

    @get:OutputDirectory
    public abstract val outputFolder: DirectoryProperty

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generateFlow() {
        workerExecutor.classLoaderIsolation {
            it.classpath.from(classpath)
        }.submit(CreateFlowGraph::class.java) {
            it.inputFiles.setFrom(sources)
            it.outputFolder.set(outputFolder)
        }
    }
}
