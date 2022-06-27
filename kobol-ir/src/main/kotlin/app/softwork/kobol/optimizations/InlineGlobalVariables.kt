package app.softwork.kobol.optimizations

import app.softwork.kobol.*

fun KobolIRTree.inlineGlobalVariables(): KobolIRTree = this
    /*
    val globalVariables = types.<KobolIRTree.Types.GlobalVariable>()
    globalVariables.map {
        this.main
    }
    return this
}

fun KobolIRTree.Types.Function.Statement.Declaration.findUsages(
    functions: List<KobolIRTree.Types.Function>
): Sequence<KobolIRTree.Types.Function.Statement.Assignment> {
    val using = functions.filter {
        it.parameters.contains(this) || it.body.any {
            it is KobolIRTree.Types.Function.Statement.Assignment && it.declaration == this
        }
    }
    if (using.size == 1) {

    }
}
*/
