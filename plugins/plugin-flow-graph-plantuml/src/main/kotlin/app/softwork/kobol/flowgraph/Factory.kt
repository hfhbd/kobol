package app.softwork.kobol.flowgraph

import app.softwork.kobol.fir.*
import java.io.*

public class Factory: FirCodeGeneratorFactory {
    override fun invoke(outputFolder: File): PlantumlFlowGraph {
        val plantumlFolder = File(outputFolder, "uml")
        return PlantumlFlowGraph(plantumlFolder)
    }
}
