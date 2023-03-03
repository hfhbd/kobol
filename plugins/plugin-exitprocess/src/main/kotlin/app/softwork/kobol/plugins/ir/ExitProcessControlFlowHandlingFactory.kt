package app.softwork.kobol.plugins.ir

import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.*
import java.io.*

public class ExitProcessControlFlowHandlingFactory : ControlFlowHandlingFactory {
    override fun invoke(): ExitProcessControlFlowHandling =
        ExitProcessControlFlowHandling

    public companion object ExitProcessControlFlowHandling : ControlFlowHandling {
        override fun goBack(goBack: GoBack): List<Statement> {
            TODO("GoBack is platform/application agnostic. Use your own ControlFlowHandling.")
        }

        override fun stopRun(stopRun: StopRun, returnCode: IntExpression): List<Statement> =
            listOf(Statement.Exit(returnCode, stopRun.comments))
    }
}
