package app.softwork.kobol.flowgraph

import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.BooleanExpression.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.NumberExpression.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.StringExpression.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import java.io.*

public class PlantumlFlowGraph(private val outputFolder: File) : FirCodeGenerator {
    private var content = mapOf<String, String>()

    override fun generate(fir: Iterable<CobolFIRTree>) {
        content = fir.associate {
            it.id.programID.lowercase() to it.createFlowGraph()
        }
    }

    override fun close() {
        for ((program, flow) in content) {
            File(outputFolder, "$program.puml").apply {
                if (!exists()) {
                    createNewFile()
                }
            }.writeText(flow)
        }
    }
}

internal fun CobolFIRTree.createFlowGraph(): String = buildString {
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
            is Add -> +"${target.toUml()} += ${value.toUml()};"
            is Sub -> +"${target.toUml()} -= ${value.toUml()};"
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
            is Write -> TODO()
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
