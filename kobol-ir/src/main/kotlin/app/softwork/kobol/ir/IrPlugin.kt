package app.softwork.kobol.ir

import java.io.Closeable

public fun interface IrPlugin: Closeable {
    public operator fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree>

    override fun close() { }
}

public fun interface IrPluginFactory<P: IrPlugin> {
    public fun create(vararg args: String): P
}
