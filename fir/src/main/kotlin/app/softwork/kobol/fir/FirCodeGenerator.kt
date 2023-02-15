package app.softwork.kobol.fir

import java.io.*

public fun interface FirCodeGenerator : Closeable {
    public fun generate(fir: Iterable<CobolFIRTree>)

    override fun close() {}
}
