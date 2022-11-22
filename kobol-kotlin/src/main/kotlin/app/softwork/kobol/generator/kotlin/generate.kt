package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.fir.*
import app.softwork.kobol.ir.*
import java.io.*

public fun generate(
    files: Set<File>,
    output: File,
    firPlugins: List<FirPlugin> = emptyList(),
    fileHandling: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String) -> SqlPrecompiler)? = null,
    irPlugins: List<IrPlugin> = emptyList()
) {
    for (ir in files.toIR(firPlugins, fileHandling, serialization, sqlPrecompiler, irPlugins)) {
        val kotlin = generate(ir)
        kotlin.writeTo(directory = File(output, "kotlin"))
    }
}
