package app.softwork.kobol

import app.softwork.kobol.generator.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.workers.*
import java.io.*

abstract class ExecuteKobol: WorkAction<ExecuteKobol.Parameters> {
    interface Parameters: WorkParameters {
        val inputFiles: ConfigurableFileCollection
        val outputFolder: DirectoryProperty
        val optimize: Property<Boolean>
    }

    override fun execute() {
        val input: Set<File> = parameters.inputFiles.files
        val outputFolder = parameters.outputFolder.get().asFile
        generate(input, outputFolder, optimize = parameters.optimize.get())
    }
}
