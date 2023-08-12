package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(IrPlugin::class)
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
            val writeUsage = tree.findWriteUsages(it.declaration)
            writeUsage.main == null && writeUsage.types.isEmpty()
        }

        return others + tree.copy(
            types = tree.types.map {
                if (it is KobolIRTree.Types.Type.GlobalVariable && it in readOnly) {
                    when (val declaration = it.declaration) {
                        is StringDeclaration -> it.copy(
                            declaration = declaration.copy(mutable = false),
                        )
                        is Declaration.Array -> it.copy(
                            declaration = declaration.copy(mutable = false),
                        )

                        is BooleanDeclaration -> it.copy(
                            declaration = declaration.copy(mutable = false),
                        )

                        is DoubleDeclaration -> it.copy(
                            declaration = declaration.copy(mutable = false),
                        )

                        is IntDeclaration.Normal -> it.copy(
                            declaration = declaration.copy(mutable = false),
                        )
                        is IntDeclaration.ReturnCode -> it.copy(
                            declaration = declaration.copy(mutable = false),
                        )

                        is ObjectDeclaration -> it.copy(
                            declaration = declaration.copy(mutable = false),
                        )
                    }
                } else {
                    it
                }
            },
        )
    }
}
