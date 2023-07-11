package app.softwork.kobol.sqldelightprecompiler

import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.DataTree.*
import app.softwork.kobol.fir.CobolFIRTree.DataTree.WorkingStorage.Elementar.Formatter.NumberType
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberVariable
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.Sql.SqlType.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Expression
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable
import app.softwork.kobol.ir.KobolIRTree.Expression.ObjectVariable
import app.softwork.kobol.ir.KobolIRTree.Expression.StringExpression
import app.softwork.kobol.ir.KobolIRTree.Types.Function
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.ir.KobolIRTree.Types.Type.*
import app.softwork.sqldelightwriter.*
import java.io.File

public class SqlDelightPrecompiler(
    dbName: String,
    private val sqFolder: File,
    private val packageName: String,
    private val fileName: String,
) : SqlPrecompiler {

    public companion object {
        public const val DB_NAME: String = "db_name"
    }

    public var files: SqFiles? = null
        private set

    private val driverType = Class(
        name = "SqlDriver",
        packageName = "app.cash.sqldelight.db",
        constructor = emptyList(),
        members = emptyList(),
        functions = emptyList(),
        doc = emptyList(),
        init = emptyList(),
        isObject = false,
    )

    private val driver = GlobalVariable(
        ObjectDeclaration(
            type = driverType,
            name = "driver",
            value = null,
            comments = emptyList(),
            mutable = false,
            private = false,
        ),
        doc = emptyList(),
    )

    private val schemaType = Class(
        "Schema",
        packageName = packageName,
        constructor = emptyList(),
        functions = listOf(
            Function(
                "migrate",
                parameters = listOf(
                    driver.declaration,
                    IntDeclaration.Normal(
                        name = "oldVersion",
                        value = null,
                        private = false,
                        mutable = false,
                        comments = emptyList(),
                        const = false,
                        length = -1,
                        isSigned = false,
                    ),
                    IntDeclaration.Normal(
                        name = "newVersion",
                        value = null,
                        private = false,
                        mutable = false,
                        comments = emptyList(),
                        const = false,
                        length = -1,
                        isSigned = false,
                    ),
                ),
            ) {},
        ),
        members = emptyList(),
        doc = emptyList(),
        init = emptyList(),
        isObject = false,
    )

    private val DB = Class(
        name = dbName,
        packageName = packageName,
        doc = emptyList(),
        functions = emptyList(),
        init = emptyList(),
        isObject = false,
        members = listOf(
            ObjectDeclaration(
                name = "Schema",
                type = schemaType,
                comments = emptyList(),
                mutable = false,
                private = false,
                value = null,
            ),
        ),
        constructor = emptyList(),
    )

    private val dbDeclaration = ObjectDeclaration(
        "DB",
        DB,
        mutable = false,
        private = false,
        value = null,
    )

    override fun convert(sqlInit: Sql): List<Statement> {
        val firstCall = files == null
        files = writeSq(packageName, existingFiles = files) {
            migrationFile(0) {
                val javaDoc = if (sqlInit.comments.isNotEmpty()) {
                    sqlInit.comments.joinToString(
                        prefix = "/**\n",
                        postfix = " */\n",
                        separator = "",
                    ) { " * $it\n" }
                } else {
                    ""
                }
                val sqlWithJavaDoc = javaDoc + sqlInit.sql
                +sqlWithJavaDoc
            }
        }
        return if (firstCall) {
            val migration = FunctionCall(
                (DB.members.single() as ObjectDeclaration).type.functions.single(),
                parameters = listOf(
                    driver,
                    IntLiteral(0),
                    IntLiteral(1),
                ),
                comments = emptyList(),
            )
            listOf(
                getDB,
                Use(dbDeclaration, Use(DB.members.single(), migration, emptyList()), emptyList()),
            )
        } else {
            emptyList()
        }
    }

    private val getDB = ObjectDeclaration(
        name = "db",
        value = FunctionCall(
            Function(
                name = dbName,
                parameters = listOf(driver.declaration),
                returnType = DB,
            ) {},
            parameters = listOf(driver),
            comments = emptyList(),
        ),
        private = false,
        mutable = false,
        comments = emptyList(),
        type = DB,
    )

    private val executeAsOne by function { }

    private val convertRegex = """\s(.)""".toRegex()
    private val String.convert
        get() = lowercase().replace(convertRegex) {
            it.groups[1]!!.value.single().uppercase()
        }.filter {
            it.isLetterOrDigit()
        }

    override fun convert(
        sql: CobolFIRTree.ProcedureTree.Statement.Sql,
        variableToIR: (CobolFIRTree.ProcedureTree.Expression.Variable) -> Expression.Variable,
        getDeclaration: (WorkingStorage.Elementar) -> Declaration,
    ): List<Statement> {
        val first = files == null
        var queryName = sql.sql.convert
        files = writeSq(packageName, existingFiles = files) {
            queryFile(name = fileName) {
                while (queryName in queryNames) {
                    queryName += "_"
                }
                query(name = queryName, kdoc = sql.comments) {
                    +"${sql.sql};"
                }
            }
        }

        val params = sql.parameter.map {
            getDeclaration(it.target)
        }
        val getQuery = Function(
            name = "${fileName}Queries.$queryName",
            parameters = params,
        ) {}

        val query = getDB use (getQuery call params.map { it.variable() })

        val executeQuery = when (sql.type) {
            Select -> query use executeAsOne()

            Delete, Execute, Insert -> query
        }.copy(comments = sql.comments)

        if (sql.updatingHostVariables.isEmpty()) {
            return if (first) {
                listOf(getDB, executeQuery)
            } else {
                listOf(executeQuery)
            }
        } else {
            val result = Class(
                name = queryName.replaceFirstChar { if (it.isLowerCase()) it.titlecaseChar() else it },
                members = sql.updatingHostVariables.map {
                    when (val result = variableToIR(it).target) {
                        is BooleanDeclaration, is ObjectDeclaration, is Declaration.Array -> error("Not yet supported")
                        is DoubleDeclaration, is IntDeclaration, is StringDeclaration -> result
                    }
                },
                isObject = false,
                packageName = fileName,
            )

            val callResult = ObjectDeclaration(
                type = result,
                comments = sql.comments,
                name = queryName,
                mutable = false,
                private = false,
                value = executeQuery.copy(comments = emptyList()),
            )

            val getResultClass = if (first) listOf(getDB, callResult) else listOf(callResult)

            val updatingHostVariables = sql.updatingHostVariables.map { hostVariable ->
                val obj = callResult.variable() as ObjectVariable
                val variable = callResult.type.members.single {
                    it.name == hostVariable.target.name
                }.variable()

                val use: Expression = when (hostVariable) {
                    is NumberVariable -> {
                        when (hostVariable.target.formatter.numberType) {
                            NumberType.Int -> IntVariable.Use(
                                obj,
                                variable as IntVariable,
                                emptyList(),
                            )

                            NumberType.Double -> DoubleVariable.Use(
                                obj,
                                variable as DoubleVariable,
                                emptyList(),
                            )
                        }
                    }

                    is StringVariable -> {
                        StringExpression.StringVariable.Use(
                            obj,
                            variable as StringExpression.StringVariable,
                        )
                    }
                }
                Assignment(
                    newValue = use,
                    declaration = getDeclaration(hostVariable.target),
                )
            }

            return getResultClass + updatingHostVariables
        }
    }

    override fun close() {
        files?.writeTo(sqFolder)
    }
}
