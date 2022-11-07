package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.ProcedureTree.Expression.BooleanExpression.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Expression.NumberExpression.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Expression.StringExpression.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import java.io.*

fun CobolFIRTree.createFlowGraph(): String = buildString {
    operator fun String.unaryPlus(): StringBuilder = appendLine(this)

    +"@startuml"
    +"start"
    procedure.topLevel.forEach {
        toUml(it)
    }
    +"end"

    procedure.sections.forEach {
        +"package ${it.name}"
        +"start"
        it.statements.forEach {
            toUml(it)
        }
        +"stop"
    }
    +"@enduml"
}

private fun StringBuilder.toUml(statement: CobolFIRTree.ProcedureTree.Statement) {
    operator fun String.unaryPlus(): StringBuilder = appendLine(this)
    with(statement) {
        when (this) {
            is Call -> +":call $name|"
            is Continue -> TODO()
            is Display -> +":DISPLAY ${expr.toUml()};"
            is Eval -> TODO()
            is ForEach -> TODO()
            is GoBack -> TODO()
            is If -> {
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

            is Move -> +":MOVE ${value.toUml()} TO ${target.toUml()};"
            is Perform -> {
                val until = until
                if (until == null) {
                    +":$sectionName|"
                } else {
                    +":$sectionName|"
                    +"while (${Not(until).toUml()})"
                    +":$sectionName|"
                    +"endwhile"
                }
            }

            is While -> TODO()
            is Sql -> TODO()
            is Read -> TODO()
            is Open -> TODO()
            is Close -> TODO()
        }
    }
}

private fun CobolFIRTree.ProcedureTree.Expression.toUml(): String = when (this) {
    is And -> "${left.toUml()} && ${right.toUml()}"
    is Equals -> "${left.toUml()} = ${right.toUml()}"
    is Greater -> {
        if (equals) "${left.toUml()} >= ${right.toUml()}" else "${left.toUml()} > ${right.toUml()}"
    }

    is Not -> "!${target.toUml()}"
    is Or -> "${left.toUml()} || ${right.toUml()}"
    is Smaller -> {
        if (equals) "${left.toUml()} <= ${right.toUml()}" else "${left.toUml()} < ${right.toUml()}"
    }

    is NumberLiteral -> value.toString()
    is StringLiteral -> "\"$value\""
    is NumberVariable -> target.toUml()
    is Concat -> "${left.toUml()} ${right.toUml()}"
    is Interpolation -> TODO()
    is StringVariable -> target.toUml()
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
