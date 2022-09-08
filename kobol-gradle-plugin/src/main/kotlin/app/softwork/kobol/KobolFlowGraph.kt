package app.softwork.kobol

import org.gradle.api.file.*
import org.gradle.api.tasks.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
abstract class KobolFlowGraph: SourceTask() {
    init {
        group = "Kobol"
    }

    @InputFiles
    @SkipWhenEmpty
    @IgnoreEmptyDirectories
    @PathSensitive(PathSensitivity.RELATIVE)
    override fun getSource(): FileTree {
        return super.getSource()
    }

    @get:OutputDirectory
    abstract val outputFolder: DirectoryProperty

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    fun generateFlow() {
        workerExecutor.classLoaderIsolation().submit(CreateFlowGraph::class.java) {
            it.inputFiles.setFrom(source)
            it.outputFolder.set(outputFolder)
        }
    }
}
