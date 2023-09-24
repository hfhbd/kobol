package app.softwork.kobol.gradle

import app.softwork.kobol.fir.*
import app.softwork.kobol.*
import org.gradle.api.file.*
import org.gradle.workers.*
import java.io.*
import java.util.*

internal abstract class FirKobolAction : WorkAction<FirKobolAction.Parameters> {
    internal interface Parameters : WorkParameters {
        val inputFiles: ConfigurableFileCollection
        val outputFolder: DirectoryProperty
    }

    override fun execute() {
        val inputs: Set<File> = parameters.inputFiles.files
        val rootPath = parameters.inputFiles.singleFile.toPath()
        val outputFolder = parameters.outputFolder.get().asFile

        val firPlugins = ServiceLoader.load(FirPluginBeforePhase::class.java) + ServiceLoader.load(
            FirPluginAfterPhase::class.java,
        )

        val firGenerators = ServiceLoader.load(FirCodeGeneratorFactory::class.java).toList()

        for (firGenerator in firGenerators) {
            firGenerator(outputFolder).use { generator ->
                generator.generate(inputs.toTree(absoluteBasePath = rootPath, firPlugins = firPlugins))
            }
        }
        for (firPlugin in firPlugins) {
            firPlugin.close()
        }
    }
}
