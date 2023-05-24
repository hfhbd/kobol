package app.softwork.kobol.fir

public fun interface FirCodeGenerator : AutoCloseable {
    public fun generate(fir: Iterable<CobolFIRTree>)

    override fun close() {}
}
