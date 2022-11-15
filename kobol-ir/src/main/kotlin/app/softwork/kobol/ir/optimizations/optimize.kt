package app.softwork.kobol.ir.optimizations

import app.softwork.kobol.ir.*

public fun KobolIRTree.optimize(): KobolIRTree {
    val after = private()
        .readonlyVariables()
        .constVariables()

    return BooleanExpressions(CamelCase(after))
}
