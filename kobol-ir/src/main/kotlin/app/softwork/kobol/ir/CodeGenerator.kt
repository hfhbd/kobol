package app.softwork.kobol.ir

import java.io.*

public fun interface CodeGenerator : Closeable {
    public fun generate(ir: Iterable<KobolIRTree>)

    override fun close() {}
}

public fun interface CodeGeneratorFactory<CG : CodeGenerator> {
    public operator fun invoke(outputFolder: File, args: Map<String, String>): CG
}
