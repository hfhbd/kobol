package app.softwork.kobol.plugins

import app.softwork.kobol.fir.*
import kotlinx.serialization.json.*
import java.io.*

public class Factory : FirCodeGeneratorFactory {
    override fun invoke(outputFolder: File): Statistics = Statistics(
        outputFolder = File(outputFolder, "statistics").apply { mkdirs() },
        format = Json {
            prettyPrint = true
        }
    )
}
