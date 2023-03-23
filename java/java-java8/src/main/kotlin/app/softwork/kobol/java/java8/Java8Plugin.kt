package app.softwork.kobol.java.java8

import app.softwork.kobol.ir.IrPlugin
import app.softwork.kobol.ir.KobolIRTree
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(IrPlugin::class)
public class Java8Plugin : IrPlugin {
    override fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree> {
        return whenToIf.invoke(tree, others)
    }
}
