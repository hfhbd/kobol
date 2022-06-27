package app.softwork.kobol.optimizations

import app.softwork.kobol.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.Declaration.*

fun KobolIRTree.readonlyVariables(): KobolIRTree {
    val variables = buildList {
        for (type in types) {
            when (type) {
                is KobolIRTree.Types.Type.GlobalVariable -> add(type)
                else -> Unit
            }
        }
    }
    val readOnly = variables.filter {
        it.declaration.findWriteUsages(
            buildList {
                for (type in types) {
                    when (type) {
                        is KobolIRTree.Types.Function -> add(type)
                        else -> Unit
                    }
                }
            } + main
        ).isNotEmpty()
    }

    return copy(types = types.map {
        if (it is KobolIRTree.Types.Type.GlobalVariable && it in readOnly) {
            when (it.declaration) {
                is StringDeclaration -> it.copy(
                    declaration = it.declaration.copy(modifier = Modifier.ReadOnly)
                )
            }
        } else it
    })
}

fun KobolIRTree.Types.Function.Statement.Declaration.findWriteUsages(
    functions: List<KobolIRTree.Types.Function>
): List<KobolIRTree.Types.Function> = functions.filter {
    it.body.any {
        it is KobolIRTree.Types.Function.Statement.Assignment && it.declaration == this
    }
}
