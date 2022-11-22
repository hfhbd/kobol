package app.softwork.kobol.ir.optimizations

import app.softwork.kobol.ir.*

public val Optimize: IrPlugin = IrPlugin { tree, others ->
    var currentTree = tree
    var trees = others.associateBy { it.id }
    val plugins = listOf(Private, ReadonlyVariables, ConstVariables, CamelCase, BooleanExpressions)
    for (plugin in plugins) {
        val others = trees - tree.id
        trees = plugin(currentTree, others.values).associateBy { it.id }
        currentTree = trees[tree.id]!!
    }
    trees.values
}
