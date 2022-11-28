package app.softwork.kobol.flowgraph

import java.io.*

public class Factory: FlowGraphFactory {
    override fun invoke(outputFolder: File): PlantumlFlowGraph = PlantumlFlowGraph(outputFolder)
}
