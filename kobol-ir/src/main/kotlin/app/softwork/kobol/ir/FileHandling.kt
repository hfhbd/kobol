package app.softwork.kobol.ir

import app.softwork.kobol.fir.*
import java.io.*

public interface FileHandling: Closeable {
    public fun handleOpen(open: CobolFIRTree.ProcedureTree.Statement.Open): List<KobolIRTree.Types.Function.Statement>
    public fun handleClose(close: CobolFIRTree.ProcedureTree.Statement.Close): List<KobolIRTree.Types.Function.Statement>

    override fun close() { }
}

public fun interface FileHandlingFactory {
    public operator fun invoke(
        packageName: String,
        args: Map<String, String>
    ): FileHandling
}
