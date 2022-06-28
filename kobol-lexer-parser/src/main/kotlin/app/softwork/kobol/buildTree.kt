package app.softwork.kobol

import com.intellij.psi.*
import com.intellij.psi.util.*

fun CobolFile.toTree(): CobolFIRTree {
    for (child in children) {
        if (child is CobolLine) {
            val program = child.program
            if (program != null) {
                val id = program.idDiv.toID()
                val env = program.envDiv?.toEnv()
                val data = program.dataDiv?.toData()
                val procedure = program.procedureDiv.toProcedure(data)

                return CobolFIRTree(
                    id = id, env = env, data = data, procedure = procedure
                )
            }
        }
        if (child is PsiErrorElement) {
            error("$child")
        }
    }
    error("No CobolLine found")
}

private fun CobolIdDiv.toID(): CobolFIRTree.ID {
    val programID = programID.varName.text
    val author = authorList.single().anys.asString()
    val installation = installationList.single().anys.asString()
    val date = dateList.single().anys.asString()

    return CobolFIRTree.ID(
        programID = programID,
        author = author,
        installation = installation,
        date = date,
    )
}

private fun CobolAnys.asString(): String {
    return siblings().joinToString("")
}

private fun CobolEnvDiv.toEnv(): CobolFIRTree.EnvTree {
    return CobolFIRTree.EnvTree
}

private fun CobolDataDiv.toData() = CobolFIRTree.DataTree(workingStorageSection?.saList?.map {
    val pic = it.pic
    when {
        pic.pic9 != null -> TODO()
        pic.picS != null -> TODO()
        pic.picXA != null -> CobolFIRTree.DataTree.WorkingStorage.Elementar.StringElementar(name = it.varName.text,
            length = it.pic.number?.text?.toInt() ?: 1,
            value = it.`var`?.let {
                it.string!!.text!!.drop(1).dropLast(1)
            })

        else -> TODO()
    }
} ?: emptyList())

private fun CobolProcedureDiv.toProcedure(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree {
    return CobolFIRTree.ProcedureTree(
        sentenceList.flatMap {
            it.proceduresList.map { it.asStatements(dataTree) }
        },
        procedureSectionList.map {
            CobolFIRTree.ProcedureTree.Section(
                name = it.varName.text,
                it.sentence.proceduresList.map {
                    it.asStatements(dataTree)
                }
            )
        }
    )
}

private fun CobolProcedures.asStatements(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Statement {
    return when {
        display != null -> CobolFIRTree.ProcedureTree.Statement.Display(
            expr = display!!.stringConcat.toExpr(dataTree)
        )

        moving != null -> CobolFIRTree.ProcedureTree.Statement.Move(
            target = dataTree!!.find(moving!!.varName)!!, value = moving!!.expr.toExpr(dataTree)
        )

        performing != null -> CobolFIRTree.ProcedureTree.Statement.Perform(
            sectionName = performing!!.varName.text
        )

        else -> TODO()
    }
}

private fun CobolExpr.toExpr(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Expression {
    return when {
        `var` != null -> when {
            `var`!!.string != null -> `var`!!.string!!.singleAsString(dataTree)
            `var`!!.number != null -> TODO()
            else -> TODO("$`var`")
        }

        varName != null -> dataTree!!.find(varName!!)!!.toVariable()
        stringConcat != null -> stringConcat!!.toExpr(dataTree)
        else -> TODO("$elementType")
    }
}

private fun PsiElement.singleAsString(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Expression.StringExpression {
    return when (elementType) {
        CobolTypes.STRING -> CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral(
            value = text.drop(1).dropLast(1)
        )

        CobolTypes.VARNAME -> CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable(
            dataTree!!.find(this) as CobolFIRTree.DataTree.WorkingStorage.Elementar.StringElementar
        )

        else -> TODO("$elementType")
    }
}

private val CobolStringConcat.allChildren: Sequence<PsiElement>
    get() = generateSequence(firstChild) {
        it.firstChild ?: it.nextSibling
    }

private fun CobolStringConcat.toExpr(dataTree: CobolFIRTree.DataTree?): CobolFIRTree.ProcedureTree.Expression.StringExpression {
    val allChildren = allChildren.toList()
    println("${allChildren.size}: ${allChildren.joinToString("")}")
    println("$firstChild $lastChild")
    require(allChildren.isNotEmpty())
    if (allChildren.count() == 1) {
        val single = allChildren.single()
        return single.singleAsString(dataTree)
    }

    val first = allChildren.first().singleAsString(dataTree)
    val s = allChildren.foldSecond(first) { acc, psi ->
        if (psi.elementType == TokenType.WHITE_SPACE) null
        else CobolFIRTree.ProcedureTree.Expression.StringExpression.Concat(
            left = acc, right = psi.singleAsString(dataTree)
        )
    }
    return s
}

inline fun <T, R> Iterable<T>.foldSecond(initial: R, operation: (acc: R, T) -> R?): R {
    var accumulator = initial
    var first = true
    for (element in this) {
        if (first) {
            first = false
        } else {
            val next = operation(accumulator, element)
            if (next != null) {
                accumulator = next
            }
        }
    }
    return accumulator
}

private fun CobolFIRTree.DataTree.find(varName: PsiElement): CobolFIRTree.DataTree.WorkingStorage.Elementar? {
    val name: String = varName.text
    return workingStorage.find { (it as? CobolFIRTree.DataTree.WorkingStorage.Elementar)?.name == name } as? CobolFIRTree.DataTree.WorkingStorage.Elementar
}

private fun CobolFIRTree.DataTree.WorkingStorage.Elementar.toVariable(): CobolFIRTree.ProcedureTree.Expression.Variable =
    when (this) {
        is CobolFIRTree.DataTree.WorkingStorage.Elementar.StringElementar -> CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable(
            target = this
        )
    }
