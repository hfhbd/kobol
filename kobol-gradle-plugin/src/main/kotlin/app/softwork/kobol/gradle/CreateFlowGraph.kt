package app.softwork.kobol.gradle

import app.softwork.kobol.fir.*
import app.softwork.kobol.flowgraph.*
import org.gradle.api.file.*
import org.gradle.workers.*
import java.io.*

public abstract class CreateFlowGraph : WorkAction<CreateFlowGraph.Parameters> {
    public interface Parameters : WorkParameters {
        public val inputFiles: ConfigurableFileCollection
        public val outputFolder: DirectoryProperty
    }

    override fun execute() {
        val inputs: Set<File> = parameters.inputFiles.files
        val outputFolder = parameters.outputFolder.get().asFile
        FlowGraph(outputFolder).use { plugin ->
            for (input in inputs) {
                plugin(input.toTree())
            }
        }
    }
}
