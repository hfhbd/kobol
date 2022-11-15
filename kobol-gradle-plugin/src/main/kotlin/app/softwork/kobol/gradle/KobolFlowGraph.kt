package app.softwork.kobol.gradle

import org.gradle.api.file.*
import org.gradle.api.tasks.*
import org.gradle.workers.*
import javax.inject.*

@CacheableTask
public abstract class KobolFlowGraph: SourceTask() {
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
    public abstract val outputFolder: DirectoryProperty

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun generateFlow() {
        workerExecutor.classLoaderIsolation().submit(CreateFlowGraph::class.java) {
            it.inputFiles.setFrom(source)
            it.outputFolder.set(outputFolder)
        }
    }
}
