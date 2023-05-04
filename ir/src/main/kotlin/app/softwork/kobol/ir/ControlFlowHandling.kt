package app.softwork.kobol.ir

import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.*
import java.io.*

public interface ControlFlowHandling : Closeable {
    public fun goBack(goBack: GoBack): List<Statement>

    public fun stopRun(
        stopRun: StopRun,
        returnCode: IntExpression,
    ): List<Statement>

    override fun close() {}
}

public fun interface ControlFlowHandlingFactory {
    public operator fun invoke(): ControlFlowHandling
}
