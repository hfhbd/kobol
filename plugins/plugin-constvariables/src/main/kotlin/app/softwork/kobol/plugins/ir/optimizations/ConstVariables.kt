package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.IrPlugin
import app.softwork.kobol.ir.KobolIRTree
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(IrPlugin::class)
public class ConstVariables : IrPlugin {
    override fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree> {
        return others + tree.copy(
            types = tree.types.map {
                it.constVariable()
            }
        )
    }
}

internal fun KobolIRTree.Types.constVariable() = when (this) {
    is KobolIRTree.Types.Type.GlobalVariable -> {
        if (declaration is Primitive && !declaration.mutable && declaration.value is KobolIRTree.Expression.Literal) {
            copy(
                declaration = when (val declaration = declaration as Primitive) {
                    is BooleanDeclaration -> declaration.copy(const = true)
                    is DoubleDeclaration -> declaration.copy(const = true)
                    is IntDeclaration -> declaration.copy(const = true)
                    is StringDeclaration -> declaration.copy(const = true)
                }
            )
        } else this
    }

    else -> this
}
