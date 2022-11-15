package app.softwork.kobol.ir.optimizations

import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*

public fun KobolIRTree.readonlyVariables(): KobolIRTree {
    val variables = buildList {
        for (type in types) {
            when (type) {
                is KobolIRTree.Types.Type.GlobalVariable -> add(type)
                else -> continue
            }
        }
    }
    val readOnly = variables.filter {
        it.declaration.findWriteUsages(
            buildList {
                for (type in types) {
                    when (type) {
                        is KobolIRTree.Types.Function -> add(type)
                        else -> continue
                    }
                }
            } + main
        ).isEmpty()
    }

    return copy(types = types.map {
        if (it is KobolIRTree.Types.Type.GlobalVariable && it in readOnly) {
            when (it.declaration) {
                is StringDeclaration -> it.copy(
                    declaration = it.declaration.copy(mutable = false)
                )

                is BooleanDeclaration -> it.copy(
                    declaration = it.declaration.copy(mutable = false)
                )

                is DoubleDeclaration -> it.copy(
                    declaration = it.declaration.copy(mutable = false)
                )

                is IntDeclaration -> it.copy(
                    declaration = it.declaration.copy(mutable = false)
                )

                is ObjectDeclaration -> it.copy(
                    declaration = it.declaration.copy(mutable = false)
                )
            }
        } else it
    })
}

internal fun KobolIRTree.Types.Function.Statement.Declaration.findWriteUsages(
    functions: List<KobolIRTree.Types.Function>
): List<KobolIRTree.Types.Function> = functions.filter {
    it.body.any {
        when {
            it is KobolIRTree.Types.Function.Statement.Assignment && it.declaration == this -> true
            it is KobolIRTree.Types.Function.Statement.For && it.counter == this -> true
            else -> false
        }
    }
}
