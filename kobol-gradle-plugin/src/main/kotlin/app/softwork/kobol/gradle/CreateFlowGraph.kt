package app.softwork.kobol.gradle

import app.softwork.kobol.fir.*
import app.softwork.kobol.flowgraph.*
import org.gradle.api.file.*
import org.gradle.workers.*
import java.io.*
import java.util.*

public abstract class CreateFlowGraph : WorkAction<CreateFlowGraph.Parameters> {
    public interface Parameters : WorkParameters {
        public val inputFiles: ConfigurableFileCollection
        public val outputFolder: DirectoryProperty
    }

    override fun execute() {
        val inputs: Set<File> = parameters.inputFiles.files
        val outputFolder = parameters.outputFolder.get().asFile

        val firPlugins = ServiceLoader.load(FirPluginBeforePhase::class.java) + ServiceLoader.load(
            FirPluginAfterPhase::class.java
        )

        inputs.toTree(firPlugins + FlowGraph(outputFolder))
    }
}
