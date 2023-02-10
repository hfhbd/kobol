package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.*

public class Private : IrPlugin {
    override fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree> {
        return others + tree.copy(types = tree.types.map { type ->
            when (type) {
                is KobolIRTree.Types.Type.GlobalVariable -> type.copy(
                    declaration = type.declaration.private()
                )

                is KobolIRTree.Types.Function -> type.copy(
                    private = true,
                    body = type.body.map {
                        when (it) {
                            is KobolIRTree.Types.Function.Statement.Assignment -> {
                                val dec = it.declaration
                                if (dec is KobolIRTree.Types.Function.Statement.Declaration) {
                                    it.copy(declaration = dec.private())
                                } else it
                            }

                            is KobolIRTree.Types.Function.Statement.For -> it.copy(counter = it.counter.private() as KobolIRTree.Types.Function.Statement.Declaration.NumberDeclaration)
                            else -> it
                        }
                    }.toMutableList()
                )

                else -> type
            }
        })
    }
}

private fun KobolIRTree.Types.Function.Statement.Declaration.private() = when (this) {
    is KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration -> copy(
        private = true
    )

    is KobolIRTree.Types.Function.Statement.Declaration.BooleanDeclaration -> copy(
        private = true
    )

    is KobolIRTree.Types.Function.Statement.Declaration.DoubleDeclaration -> copy(
        private = true
    )

    is KobolIRTree.Types.Function.Statement.Declaration.IntDeclaration -> copy(
        private = true
    )

    is KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration -> copy(
        private = true
    )
}