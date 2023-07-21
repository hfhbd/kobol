package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(IrPlugin::class)
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
                                if (dec is Declaration) {
                                    it.copy(declaration = dec.private())
                                } else it
                            }

                            is KobolIRTree.Types.Function.Statement.For -> it.copy(counter = it.counter.private() as Declaration.NumberDeclaration)
                            else -> it
                        }
                    }.toMutableList()
                )

                else -> type
            }
        })
    }
}

private fun Declaration.private() = when (this) {
    is Declaration.StringDeclaration -> copy(
        private = true
    )

    is Declaration.BooleanDeclaration -> copy(
        private = true
    )

    is Declaration.DoubleDeclaration -> copy(
        private = true
    )

    is Declaration.IntDeclaration.Normal -> copy(
        private = true
    )
    is Declaration.IntDeclaration.ReturnCode -> this

    is Declaration.ObjectDeclaration -> copy(
        private = true
    )
    is Declaration.Array -> copy(
        private = true
    )
}
