package app.softwork.kobol.java.java8

import app.softwork.kobol.ir.*

public class Java8Plugin : IrPlugin {
    override fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree> {
        return whenToIf.invoke(tree, others)
    }
}
