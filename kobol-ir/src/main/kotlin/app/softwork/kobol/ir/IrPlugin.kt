package app.softwork.kobol.ir

import java.io.Closeable

public fun interface IrPlugin: Closeable {
    public operator fun invoke(tree: KobolIRTree): KobolIRTree

    override fun close() { }
}
