package app.softwork.kobol

import org.gradle.api.file.*
import org.gradle.workers.*
import java.io.*

abstract class CreateFlowGraph: WorkAction<CreateFlowGraph.Parameters> {
    interface Parameters: WorkParameters {
        val inputFiles: ConfigurableFileCollection
        val outputFolder: DirectoryProperty
    }

    override fun execute() {
        val input: Set<File> = parameters.inputFiles.files
        val outputFolder = parameters.outputFolder.get().asFile
        flowGraph(input, outputFolder)
    }
}
