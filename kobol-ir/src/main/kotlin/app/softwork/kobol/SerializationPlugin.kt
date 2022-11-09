package app.softwork.kobol

import java.io.*

interface SerializationPlugin: Closeable {
    /**
     * Converts the records of the file section into classes
     */
    fun fileSection(fileSection: CobolFIRTree.DataTree.File): List<KobolIRTree.Types.Type.Class>

    /**
     * Converts the lines of the file from [FileHandling]
     */
    fun readSequence(
        read: CobolFIRTree.ProcedureTree.Statement.Read,
        toIR: List<CobolFIRTree.ProcedureTree.Statement>.() -> List<KobolIRTree.Types.Function.Statement>
    ): List<KobolIRTree.Types.Function.Statement>

    fun write(
        write: CobolFIRTree.ProcedureTree.Statement.Write
    ): List<KobolIRTree.Types.Function.Statement>

    override fun close() { }
}
