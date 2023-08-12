package app.softwork.kobol.fir

import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.*

public fun List<Statement>.visit(action: Statement.() -> Statement?): List<Statement> = mapNotNull {
    val visitInnerStatements = action(it) ?: return@mapNotNull null
    when (visitInnerStatements) {
        is Statement.Add,
        is Statement.Close,
        is Statement.Continue,
        is Statement.Display,
        is Statement.GoBack,
        is Statement.StopRun,
        is Statement.Move,
        is Statement.Open,
        is Statement.Sql,
        is Statement.Write,
        is Statement.Perform,
        is Statement.Sub,
        is Statement.Call,
        -> visitInnerStatements

        is Statement.Read -> visitInnerStatements.copy(
            action = visitInnerStatements.action.visit(action),
            atEnd = visitInnerStatements.atEnd.visit(action),
        )

        is Statement.Eval -> visitInnerStatements.copy(
            conditions = visitInnerStatements.conditions.map { cond ->
                cond.copy(
                    action = cond.action.visit(action),
                )
            },
            other = visitInnerStatements.other?.copy(
                action = visitInnerStatements.other.action.visit(action),
            ),
        )

        is Statement.ForEach -> visitInnerStatements.copy(
            statements = visitInnerStatements.statements.visit(action),
        )

        is Statement.If -> visitInnerStatements.copy(
            statements = visitInnerStatements.statements.visit(action),
            elseStatements = visitInnerStatements.elseStatements.visit(action),
        )

        is Statement.While -> visitInnerStatements.copy(
            statements = visitInnerStatements.statements.visit(action),
        )
    }
}
