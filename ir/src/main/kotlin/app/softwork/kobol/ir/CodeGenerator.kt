package app.softwork.kobol.ir

import java.io.File

public fun interface CodeGenerator : AutoCloseable {
    public fun generate(ir: Iterable<KobolIRTree>)

    override fun close() {}
}

public fun interface CodeGeneratorFactory {
    public operator fun invoke(outputFolder: File, args: Map<String, String>): CodeGenerator
}
