package app.softwork.kobol.flowgraph

import app.softwork.kobol.fir.*
import app.softwork.serviceloader.ServiceLoader
import java.io.*

@ServiceLoader(FirCodeGeneratorFactory::class)
public class Factory : FirCodeGeneratorFactory {
    override fun invoke(outputFolder: File): PlantumlFlowGraph {
        val plantumlFolder = File(outputFolder, "uml").apply {
            mkdirs()
        }
        return PlantumlFlowGraph(plantumlFolder)
    }
}
