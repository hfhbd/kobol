package app.softwork.kobol.sqldelightprecompiler

import app.softwork.kobol.*
import app.softwork.kobol.CobolFIRTree.DataTree.*
import app.softwork.kobol.KobolIRTree.Types.Function
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.KobolIRTree.Types.Type.*
import app.softwork.sqldelightwriter.*
import java.io.*

class SqlDelightPrecompiler(
    val dbName: String,
    val sqFolder: File,
    val packageName: String,
    val fileName: String
) : SqlPrecompiler {
    private var files: SqFiles? = null

    override fun generatedTypes(): List<KobolIRTree.Types> = generatedTypes + driver

    private val generatedTypes = mutableListOf<KobolIRTree.Types>()

    private val driverType = Class(
        name = "SqlDriver",
        packageName = "app.cash.sqldelight.db",
        constructor = Class.Constructor(emptyList()),
        members = emptyList(),
        functions = emptyList(),
        doc = emptyList(),
        init = emptyList(),
        isObject = false
    )

    private val driver = GlobalVariable(
        ObjectDeclaration(
            type = driverType,
            name = "driver",
            className = null,
            value = null,
            comments = emptyList(),
            mutable = false,
            private = false,
        ), doc = emptyList()
    )

    private val db = Class(
        name = dbName,
        packageName = packageName,
        doc = emptyList(),
        functions = emptyList(),
        init = emptyList(),
        isObject = false,
        members = emptyList(),
        constructor = Class.Constructor(
            emptyList()
        )
    )

    override fun convert(sqlInit: WorkingStorage.Sql): List<Function.Statement> {
        val firstCall = files == null
        files = writeSq(packageName, existingFiles = files) {
            migrationFile(1) {
                val javaDoc = sqlInit.comments.joinToString(
                    prefix = "/**",
                    postfix = " */${System.lineSeparator()}",
                    separator = "\n"
                ) { " * $it" }
                val sqlWithJavaDoc = javaDoc + sqlInit.sql
                +sqlWithJavaDoc
            }
        }
        return if (firstCall) {
            val function = Function(
                name = dbName, parameters = listOf(driver.declaration), returnType = db
            ) {}
            listOf(
                FunctionCall(
                    function, parameters = listOf(driver.declaration), comments = emptyList()
                )
            )
        } else {
            emptyList()
        }
    }

    private val convertRegex = """\s(.)""".toRegex()

    private val invalid = """[,:.]""".toRegex()
    private val String.convert
        get() = lowercase().replace(convertRegex) {
            it.groups[1]!!.value.single().uppercase()
        }.replace(invalid, "")

    override fun convert(
        sql: CobolFIRTree.ProcedureTree.Statement.Sql,
        variableToIR: (CobolFIRTree.ProcedureTree.Expression.Variable) -> KobolIRTree.Expression.Variable,
        getDeclaration: (WorkingStorage.Elementar) -> Declaration
    ): List<Function.Statement> {
        var queryName = sql.sql.convert
        files = writeSq(packageName, existingFiles = files) {
            queryFile(name = fileName) {
                while (queryName in queryNames) {
                    queryName += "_"
                }
                query(name = queryName, kdoc = sql.comments) {
                    +sql.sql
                }
            }
        }

        val params = sql.parameter.map {
            getDeclaration(it.target)
        }
        val function = Function(
            name = "${fileName}Queries.$queryName",
            parameters = params,
        ) {}

        val query = FunctionCall(function, params, emptyList())
        val call = FunctionCall.Fluent(
            query, FunctionCall(
                function = Function(name = "executeAsOne", parameters = emptyList()) {},
                parameters = emptyList(),
                comments = emptyList()
            ), comments = sql.comments
        )

        return if (sql.hostVariables.isEmpty()) {
            listOf(call)
        } else {
            val result = Class(
                name = queryName,
                constructor = Class.Constructor(emptyList()),
                functions = emptyList(),
                members = sql.hostVariables.map {
                    when (val result = variableToIR(it).target) {
                        is BooleanDeclaration, is ObjectDeclaration -> error("Not yet supported")
                        is DoubleDeclaration -> result.copy(className = queryName)
                        is IntDeclaration -> result.copy(className = queryName)
                        is StringDeclaration -> result.copy(className = queryName)
                    }
                },
                isObject = false,
                init = emptyList(),
                doc = emptyList(),
                packageName = null
            )

            val callResult = ObjectDeclaration(
                type = result,
                comments = sql.comments,
                name = queryName,
                mutable = false,
                private = false,
                value = call,
                className = null
            )
            listOf(callResult) + sql.hostVariables.map { hostVariable ->
                Assignment(
                    comments = emptyList(),
                    newValue = callResult.type.members.single { it.name == hostVariable.target.name }.variable(),
                    declaration = getDeclaration(hostVariable.target)
                )
            }
        }
    }

    override fun close() {
        files?.writeTo(sqFolder)
    }
}
