package app.softwork.kobol

import java.io.*

interface FileHandling: Closeable {
    fun handleOpen(open: CobolFIRTree.ProcedureTree.Statement.Open): List<KobolIRTree.Types.Function.Statement>
    fun handleClose(close: CobolFIRTree.ProcedureTree.Statement.Close): List<KobolIRTree.Types.Function.Statement>

    override fun close() { }
}
