package app.softwork.kobol

import java.io.*

fun interface FirPlugin: Closeable {
    operator fun invoke(tree: CobolFIRTree): CobolFIRTree

    override fun close() { }
}
