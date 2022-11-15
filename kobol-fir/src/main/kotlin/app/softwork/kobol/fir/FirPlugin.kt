package app.softwork.kobol.fir

import java.io.*

public fun interface FirPlugin: Closeable {
    public operator fun invoke(tree: CobolFIRTree): CobolFIRTree

    override fun close() { }
}
