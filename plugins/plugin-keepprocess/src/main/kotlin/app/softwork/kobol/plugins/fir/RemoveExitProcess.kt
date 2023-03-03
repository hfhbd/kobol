package app.softwork.kobol.plugins.fir

import app.softwork.kobol.fir.*

public class RemoveExitProcess : FirPluginBeforePhase {
    override fun invoke(tree: CobolFIRTree): CobolFIRTree {
        return tree.copy(
            procedure = tree.procedure.copy(
                topLevel = tree.procedure.topLevel.visit { removeExit() },
                sections = tree.procedure.sections.map {
                    it.copy(statements = it.statements.visit { removeExit() })
                }
            )
        )
    }
}

private fun CobolFIRTree.ProcedureTree.Statement.removeExit(): CobolFIRTree.ProcedureTree.Statement? {
    return when (this) {
        is CobolFIRTree.ProcedureTree.Statement.GoBack -> null

        else -> this
    }
}
