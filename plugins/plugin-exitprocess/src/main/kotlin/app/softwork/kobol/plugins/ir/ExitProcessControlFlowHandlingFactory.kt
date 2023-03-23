package app.softwork.kobol.plugins.ir

import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.GoBack
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.StopRun
import app.softwork.kobol.ir.ControlFlowHandling
import app.softwork.kobol.ir.ControlFlowHandlingFactory
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(ControlFlowHandlingFactory::class)
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
