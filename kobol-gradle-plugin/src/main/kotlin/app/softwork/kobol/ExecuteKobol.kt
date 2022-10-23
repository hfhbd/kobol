package app.softwork.kobol

import app.softwork.kobol.generator.kotlin.*
import app.softwork.kobol.sqldelightprecompiler.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.workers.*
import java.io.*

abstract class ExecuteKobol : WorkAction<ExecuteKobol.Parameters> {
    interface Parameters : WorkParameters {
        val inputFiles: ConfigurableFileCollection
        val outputFolder: DirectoryProperty
        val sqlFolder: DirectoryProperty
        val optimize: Property<Boolean>
    }

    override fun execute() {
        val input: Set<File> = parameters.inputFiles.files
        val outputFolder = parameters.outputFolder.get().asFile
        val sqlFolder = parameters.sqlFolder.orNull?.asFile

        generate(
            files = input,
            output = outputFolder,
            optimize = parameters.optimize.get(),
            sqlPrecompiler = sqlFolder?.let { file ->
                {
                    SqlDelightPrecompiler(
                        dbName = "DB",
                        sqFolder = file,
                        packageName = it,
                        fileName = it,
                    )
                }
            }
        )
    }
}
