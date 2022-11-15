package app.softwork.kobol.gradle

import app.softwork.kobol.sqldelightprecompiler.*
import app.softwork.kobol.generator.kotlin.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.workers.*
import java.io.*

public abstract class ExecuteKobol : WorkAction<ExecuteKobol.Parameters> {
    public interface Parameters : WorkParameters {
        public val inputFiles: ConfigurableFileCollection
        public val outputFolder: DirectoryProperty
        public val sqlFolder: DirectoryProperty
        public val optimize: Property<Boolean>
    }

    override fun execute() {
        val input: Set<File> = parameters.inputFiles.files
        val outputFolder = parameters.outputFolder.get().asFile
        val sqlFolder = parameters.sqlFolder.orNull?.asFile

        generate(
            firPlugins = emptyList(),
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
