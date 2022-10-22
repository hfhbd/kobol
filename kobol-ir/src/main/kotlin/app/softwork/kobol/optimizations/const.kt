package app.softwork.kobol.optimizations

import app.softwork.kobol.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.Declaration.*

fun KobolIRTree.constVariables(): KobolIRTree = copy(
    types = types.map {
        it.constVariable()
    }
)

internal fun KobolIRTree.Types.constVariable() = when (this) {
    is KobolIRTree.Types.Type.GlobalVariable -> {
        if (declaration is Primitive && !declaration.mutable && declaration.value is KobolIRTree.Expression.Literal) {
            val newDeclaration = when(declaration) {
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
