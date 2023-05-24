package app.softwork.kobol.ir

public fun interface IrPlugin: AutoCloseable {
    public operator fun invoke(tree: KobolIRTree, others: Iterable<KobolIRTree>): Iterable<KobolIRTree>

    override fun close() { }
}

public fun interface IrPluginFactory<P: IrPlugin> {
    public fun create(vararg args: String): P
}
