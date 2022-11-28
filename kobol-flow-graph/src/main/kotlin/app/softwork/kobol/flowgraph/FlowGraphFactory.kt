package app.softwork.kobol.flowgraph

import java.io.*

public fun interface FlowGraphFactory {
    public operator fun invoke(outputFolder: File): FlowGraph
}
