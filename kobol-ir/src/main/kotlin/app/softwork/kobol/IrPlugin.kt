package app.softwork.kobol

import java.io.Closeable

fun interface IrPlugin: Closeable {
    operator fun invoke(tree: KobolIRTree): KobolIRTree

    override fun close() { }
}
