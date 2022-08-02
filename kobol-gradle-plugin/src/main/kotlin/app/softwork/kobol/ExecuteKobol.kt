package app.softwork.kobol

import app.softwork.kobol.generator.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.workers.*

abstract class ExecuteKobol: WorkAction<ExecuteKobol.Parameters> {
    interface Parameters: WorkParameters {
        val inputFile: RegularFileProperty
        val outputFolder: DirectoryProperty
        val optimize: Property<Boolean>
    }

    override fun execute() {
        val input = parameters.inputFile.get().asFile
        val outputFolder = parameters.outputFolder.get().asFile
        generate(input, outputFolder, optimize = parameters.optimize.orNull ?: false)
    }
}
