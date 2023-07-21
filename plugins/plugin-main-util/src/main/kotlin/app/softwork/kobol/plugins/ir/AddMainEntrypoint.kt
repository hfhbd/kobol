package app.softwork.kobol.plugins.ir

import app.softwork.kobol.Builder
import app.softwork.kobol.ir.KobolIRTree
import app.softwork.kobol.ir.KobolIRTree.Types.Function
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.Array
import app.softwork.kobol.ir.KobolIRTree.Types.Type.Natives
import app.softwork.kobol.ir.function

internal val stringArg = Array(
    name = "args",
    type = Natives.String,
    nullable = false,
    mutable = false,
    private = false,
)

internal val stringArgs = listOf(stringArg)

public fun KobolIRTree.addMainEntrypoint(callMain: Builder<Function.Statement>.(Function, Array) -> Unit): KobolIRTree {
    for (type in types) {
        if (type is Function) {
            if (type.name == "main") {
                val singleParam = type.parameters.singleOrNull()
                if (singleParam != null && singleParam is Array && singleParam.type is Natives.String) {
                    error("$this already contains a main function with String array paramenters.")
                }
            }
        }
    }
    val newMain = function("main") {
        callMain(main, stringArg)
    }.copy(parameters = stringArgs)

    return copy(types = types + newMain)
}
