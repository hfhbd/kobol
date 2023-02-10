package app.softwork.kobol.flowgraph

import app.softwork.kobol.fir.*
import java.io.*

public interface FlowGraph: FirPluginAfterPhase {
    public val outputFolder: File
    override fun close()
}
