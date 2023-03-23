package app.softwork.kobol.sqldelightprecompiler

import app.softwork.kobol.ir.*
import app.softwork.kobol.sqldelightprecompiler.SqlDelightPrecompiler.Companion.DB_NAME
import app.softwork.serviceloader.ServiceLoader
import java.io.*

@ServiceLoader(SqlPrecompilerFactory::class)
public class Factory : SqlPrecompilerFactory {
    override operator fun invoke(
        packageName: String,
        fileName: String,
        outputFolder: File?,
        args: Map<String, String>
    ): SqlDelightPrecompiler {
        return SqlDelightPrecompiler(
            dbName = args[DB_NAME]!!,
            sqFolder = outputFolder!!,
            packageName = packageName,
            fileName = fileName
        )
    }
}
