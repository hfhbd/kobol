package app.softwork.kobol.ir

import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.Close
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.Open
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement

public interface FileHandling : AutoCloseable {
    public fun handleOpen(open: Open): List<Statement>
    public fun handleClose(close: Close): List<Statement>

    override fun close() { }
}
