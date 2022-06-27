package app.softwork.kobol.optimizations

import app.softwork.kobol.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.Declaration.*

fun KobolIRTree.constVariables(): KobolIRTree = copy(
    types = types.map {
        it.constVariable()
    }
)

private fun KobolIRTree.Types.constVariable() = when (this) {
    is KobolIRTree.Types.Type.GlobalVariable -> {
        if (declaration is Primitive && !declaration.mutable && declaration.value is KobolIRTree.Expression.Literal) {
            copy(const = true)
        } else this
    }

    else -> this
}
