package app.softwork.kobol.ir

import app.softwork.kobol.fir.*

public interface SerializationPlugin : AutoCloseable {
    /**
     * Converts the records of the file section into classes
     */
    public fun fileSection(fileSection: CobolFIRTree.DataTree.File): List<KobolIRTree.Types.Type.Class>

    /**
     * Converts the lines of the file from [FileHandling]
     */
    public fun readSequence(
        read: CobolFIRTree.ProcedureTree.Statement.Read,
        toIR: List<CobolFIRTree.ProcedureTree.Statement>.() -> List<KobolIRTree.Types.Function.Statement>,
    ): List<KobolIRTree.Types.Function.Statement>

    public fun write(
        write: CobolFIRTree.ProcedureTree.Statement.Write,
    ): List<KobolIRTree.Types.Function.Statement>

    override fun close() { }
}

public fun interface SerializationPluginFactory {
    public operator fun invoke(
        packageName: String,
        args: Map<String, String>,
    ): SerializationPlugin
}
