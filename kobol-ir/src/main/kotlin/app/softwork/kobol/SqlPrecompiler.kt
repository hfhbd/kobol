package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.DataTree.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
import java.io.*

interface SqlPrecompiler : Closeable {
    fun convert(sqlInit: WorkingStorage.Sql): List<KobolIRTree.Types.Function.Statement>
    fun convert(
        sql: Statement.Sql,
        variableToIR: (Expression.Variable) -> KobolIRTree.Expression.Variable,
        getDeclaration: (WorkingStorage.Elementar) -> Declaration
    ): List<KobolIRTree.Types.Function.Statement>

    fun generatedTypes(): List<KobolIRTree.Types>
}
