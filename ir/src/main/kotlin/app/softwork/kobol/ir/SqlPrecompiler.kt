package app.softwork.kobol.ir

import app.softwork.kobol.fir.*
import java.io.*

public interface SqlPrecompiler : AutoCloseable {
    public fun convert(sqlInit: CobolFIRTree.DataTree.WorkingStorage.Sql): List<KobolIRTree.Types.Function.Statement>

    public fun convert(
        sql: CobolFIRTree.ProcedureTree.Statement.Sql,
        variableToIR: (CobolFIRTree.ProcedureTree.Expression.Variable) -> KobolIRTree.Expression.Variable,
        getDeclaration: (CobolFIRTree.DataTree.WorkingStorage.Elementar) -> KobolIRTree.Types.Function.Statement.Declaration
    ): List<KobolIRTree.Types.Function.Statement>
}

public fun interface SqlPrecompilerFactory {
    public operator fun invoke(
        packageName: String,
        fileName: String,
        outputFolder: File?,
        args: Map<String, String>
    ): SqlPrecompiler
}
