package app.softwork.kobol.optimizations

import app.softwork.kobol.*

fun KobolIRTree.private(): KobolIRTree = copy(types = types.map { type ->
    when (type) {
        is KobolIRTree.Types.Type.GlobalVariable -> type.copy(
            declaration = type.declaration.private()
        )

        is KobolIRTree.Types.Function -> type.copy(
            private = true,
            body = type.body.map {
                when (it) {
                    is KobolIRTree.Types.Function.Statement.Assignment -> it.copy(declaration = it.declaration.private())
                    is KobolIRTree.Types.Function.Statement.ForEach -> it.copy(counter = it.counter.private() as KobolIRTree.Types.Function.Statement.Declaration.NumberDeclaration)
                    else -> it
                }
            }
        )

        else -> type
    }
})

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
}
