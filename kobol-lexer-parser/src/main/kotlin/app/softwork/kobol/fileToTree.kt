package app.softwork.kobol

import java.io.*

fun File.toTree(): CobolFIRTree {
    return toCobolFile().toTree()
}

fun File.toCobolFile(): CobolFile {
    val intelliJ = CoreEnvironment(listOf(this)).apply {
        initializeApplication {
            registerFileType(CobolFileType, CobolFileType.defaultExtension)
            registerParserDefinition(CobolParserDefinition)
        }
    }
    lateinit var file: CobolFile
    intelliJ.forSourceFile<CobolFile> {
        file = it
    }
    return file
}
