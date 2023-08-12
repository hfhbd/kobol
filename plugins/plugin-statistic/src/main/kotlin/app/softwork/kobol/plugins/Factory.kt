package app.softwork.kobol.plugins

import app.softwork.kobol.fir.*
import app.softwork.serviceloader.ServiceLoader
import kotlinx.serialization.json.*
import java.io.*

@ServiceLoader(FirCodeGeneratorFactory::class)
public class Factory : FirCodeGeneratorFactory {
    override fun invoke(outputFolder: File): Statistics = Statistics(
        outputFolder = File(outputFolder, "statistics").apply { mkdirs() },
        format = Json {
            prettyPrint = true
        },
    )
}
