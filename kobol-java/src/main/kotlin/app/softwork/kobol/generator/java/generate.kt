package app.softwork.kobol.generator.java

import app.softwork.kobol.fir.*
import app.softwork.kobol.ir.*
import java.io.*

public fun generate(
    files: Set<File>,
    output: File,
    java8: Boolean,
    firPlugins: List<FirPlugin> = emptyList(),
    fileHandling: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String) -> SqlPrecompiler)? = null,
    irPlugins: List<IrPlugin> = emptyList()
) {
    val plugins = if (java8) { irPlugins + whenToIf } else irPlugins

    for (ir in files.toIR(
        firPlugins,
        fileHandling,
        serialization,
        sqlPrecompiler,
        plugins
    )) {
        generateJava(ir).forEach {
            it.writeTo(File(output, "java"))
        }
    }
}
