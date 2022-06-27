package app.softwork.kobol.optimizations

import app.softwork.kobol.*

fun KobolIRTree.optimize() =
    private()
        .readonlyVariables()
        .constVariables()
        .camelCase()
