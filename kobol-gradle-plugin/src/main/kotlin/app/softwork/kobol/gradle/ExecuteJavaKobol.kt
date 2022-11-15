package app.softwork.kobol.gradle

import app.softwork.kobol.generator.java.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.workers.*
import java.io.*

public abstract class ExecuteJavaKobol: WorkAction<ExecuteJavaKobol.Parameters> {
    public interface Parameters: WorkParameters {
        public val inputFiles: ConfigurableFileCollection
        public val outputFolder: DirectoryProperty
        public val optimize: Property<Boolean>
        public val java8: Property<Boolean>
    }

    override fun execute() {
        val input: Set<File> = parameters.inputFiles.files
        val outputFolder = parameters.outputFolder.get().asFile
        generate(input, outputFolder, optimize = parameters.optimize.get(), java8 = parameters.java8.get())
    }
}
