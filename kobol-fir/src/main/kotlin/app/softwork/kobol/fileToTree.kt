package app.softwork.kobol

import java.io.*

fun File.toTree(): CobolFIRTree? {
    val intelliJ = CoreEnvironment(listOf(this)).apply {
        initializeApplication {
            registerFileType(CobolFileType, CobolFileType.defaultExtension)
            registerParserDefinition(CobolParserDefinition)
        }
    }
    var tree: CobolFIRTree? = null
    intelliJ.forSourceFile<CobolFile> {
        tree = it.toTree()
    }
    return tree
}
