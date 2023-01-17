package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*

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
            val declaration = declaration as Primitive
            val newDeclaration = when (declaration) {
                is BooleanDeclaration -> declaration.copy(const = true)
                is DoubleDeclaration -> declaration.copy(const = true)
                is IntDeclaration -> declaration.copy(const = true)
                is StringDeclaration -> declaration.copy(const = true)
            }
            copy(declaration = newDeclaration)
        } else this
    }

    else -> this
}
