package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.*

public class Objects: IrPlugin {
    override fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree> {
        return others + tree
    }
}