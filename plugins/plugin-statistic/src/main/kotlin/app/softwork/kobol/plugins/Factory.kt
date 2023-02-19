package app.softwork.kobol.plugins

import app.softwork.kobol.fir.*
import java.io.*

public class Factory : FirCodeGeneratorFactory {
    override fun invoke(outputFolder: File): Statistics = Statistics(File(outputFolder, "statistics").apply { mkdirs() })
}
