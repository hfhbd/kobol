package app.softwork.kobol.gradle

import app.softwork.kobol.fir.*
import org.gradle.api.file.*
import org.gradle.workers.*
import java.io.*
import java.util.*

public abstract class FirKobolAction : WorkAction<FirKobolAction.Parameters> {
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
        
        for (firGenerator in ServiceLoader.load(FirCodeGeneratorFactory::class.java)) {
            val generator = firGenerator(outputFolder)
            generator.generate(inputs.toTree(firPlugins))
        }
    }
}
