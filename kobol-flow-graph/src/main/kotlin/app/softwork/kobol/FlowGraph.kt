package app.softwork.kobol

import java.io.*

fun CobolFIRTree.createFlowGraph(): String = buildString {
    operator fun String.unaryPlus(): StringBuilder = appendLine(this)

    +"@startuml"
    +"start"
    procedure.topLevel.forEach {
        toUml(it)
    }
    procedure.sections.forEach {
        it.statements.forEach {
            toUml(it)
        }
    }
    +"end"
    +"@enduml"
}

private fun StringBuilder.toUml(statement: CobolFIRTree.ProcedureTree.Statement) {
    operator fun String.unaryPlus(): StringBuilder = appendLine(this)
    with(statement) {
        when (this) {
            is CobolFIRTree.ProcedureTree.Statement.Call -> TODO()
            is CobolFIRTree.ProcedureTree.Statement.Continue -> TODO()
            is CobolFIRTree.ProcedureTree.Statement.Display -> +":DISPLAY ${expr.toUml()};"
            is CobolFIRTree.ProcedureTree.Statement.Eval -> TODO()
            is CobolFIRTree.ProcedureTree.Statement.ForEach -> TODO()
            is CobolFIRTree.ProcedureTree.Statement.GoBack -> TODO()
            is CobolFIRTree.ProcedureTree.Statement.If -> {
                +"if (${condition.toUml()}) then"
                for (statement in statements) {
                    toUml(statement)
                }
                if (elseStatements.isNotEmpty()) {
                    +"else"
                    for (elseStatement in elseStatements) {
                        toUml(elseStatement)
                    }
                }
                +"endif"
            }

            is CobolFIRTree.ProcedureTree.Statement.Move -> +":MOVE ${value.toUml()} TO ${target.toUml()};"
            is CobolFIRTree.ProcedureTree.Statement.Perform -> TODO()
            is CobolFIRTree.ProcedureTree.Statement.While -> TODO()
            is CobolFIRTree.ProcedureTree.Statement.Sql -> TODO()
        }
    }
}

private fun CobolFIRTree.ProcedureTree.Expression.toUml(): String = when (this) {
    is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.And -> "${left.toUml()} && ${right.toUml()}"
    is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Equals -> "${left.toUml()} = ${right.toUml()}"
    is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Greater -> {
        if (equals) "${left.toUml()} >= ${right.toUml()}" else "${left.toUml()} > ${right.toUml()}"
    }

    is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Not -> "!${target.toUml()}"
    is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Or -> "${left.toUml()} || ${right.toUml()}"
    is CobolFIRTree.ProcedureTree.Expression.BooleanExpression.Smaller -> {
        if (equals) "${left.toUml()} <= ${right.toUml()}" else "${left.toUml()} < ${right.toUml()}"
    }

    is CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberLiteral -> value.toString()
    is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral -> "\"$value\""
    is CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberVariable -> target.toUml()
    is CobolFIRTree.ProcedureTree.Expression.StringExpression.Concat -> "${left.toUml()} ${right.toUml()}"
    is CobolFIRTree.ProcedureTree.Expression.StringExpression.Interpolation -> TODO()
    is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable -> target.toUml()
}

private fun CobolFIRTree.DataTree.WorkingStorage.Elementar.toUml(): String {
    val recordName: String? = recordName
    return if (recordName != null) {
        "$name OF $recordName"
    } else name
}

fun flowGraph(input: Set<File>, outputFolder: File) {
    input.toTree().forEach {
        File(outputFolder, "${it.id.programID.lowercase()}.puml").writeText(it.createFlowGraph())
    }
}
