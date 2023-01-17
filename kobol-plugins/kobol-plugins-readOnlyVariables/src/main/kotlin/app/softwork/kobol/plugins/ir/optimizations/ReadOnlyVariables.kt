package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*

public class ReadOnlyVariables : IrPlugin {
    override fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree> {
        val variables = buildList {
            for (type in tree.types) {
                when (type) {
                    is KobolIRTree.Types.Type.GlobalVariable -> add(type)
                    else -> continue
                }
            }
        }
        val readOnly = variables.filter {
            it.declaration.findWriteUsages(
                buildList {
                    for (type in tree.types) {
                        when (type) {
                            is KobolIRTree.Types.Function -> add(type)
                            else -> continue
                        }
                    }
                } + tree.main
            ).isEmpty()
        }

        return others + tree.copy(types = tree.types.map {
            if (it is KobolIRTree.Types.Type.GlobalVariable && it in readOnly) {
                when (val declaration = it.declaration) {
                    is StringDeclaration -> it.copy(
                        declaration = declaration.copy(mutable = false)
                    )

                    is BooleanDeclaration -> it.copy(
                        declaration = declaration.copy(mutable = false)
                    )

                    is DoubleDeclaration -> it.copy(
                        declaration = declaration.copy(mutable = false)
                    )

                    is IntDeclaration -> it.copy(
                        declaration = declaration.copy(mutable = false)
                    )

                    is ObjectDeclaration -> it.copy(
                        declaration = declaration.copy(mutable = false)
                    )
                }
            } else it
        })
    }
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
