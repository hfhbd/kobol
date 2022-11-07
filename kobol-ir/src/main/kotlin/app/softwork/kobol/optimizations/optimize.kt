package app.softwork.kobol.optimizations

import app.softwork.kobol.*

fun KobolIRTree.optimize(): KobolIRTree {
    val after = private()
        .readonlyVariables()
        .constVariables()
        .camelCase()
    return BooleanExpressions(after)
}
