package app.softwork.kobol.sqldelightprecompiler

import app.softwork.kobol.*
import app.softwork.kobol.CobolFIRTree.DataTree.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.Sql.SqlType.*
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
            value = null,
            comments = emptyList(),
            mutable = false,
            private = false,
        ), doc = emptyList()
    )

    private val schemaType = Class(
        "Schema",
        packageName = packageName,
        constructor = Class.Constructor(emptyList()),
        functions = listOf(
            Function(
                "migrate", parameters = listOf(
                    driver.declaration,
                    IntDeclaration(
                        name = "oldVersion",
                        value = null,
                        private = false,
                        mutable = false,
                        comments = emptyList(),
                        const = false
                    ),
                    IntDeclaration(
                        name = "newVersion",
                        value = null,
                        private = false,
                        mutable = false,
                        comments = emptyList(),
                        const = false
                    )
                )
            ) {}
        ),
        members = emptyList(),
        doc = emptyList(),
        init = emptyList(),
        isObject = false
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
                value = null
            )
        ),
        constructor = Class.Constructor(
            emptyList()
        )
    )

    private val dbDeclaration = ObjectDeclaration(
        DB,
        emptyList(),
        "DB",
        mutable = false,
        private = false,
        value = null
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
            val migration = FunctionCall(
                (DB.members.single() as ObjectDeclaration).type.functions.single(),
                parameters = listOf(
                    driver,
                    KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(0),
                    KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(1)
                ),
                comments = emptyList()
            )
            listOf(
                db,
                Use(dbDeclaration, Use(DB.members.single(), migration, emptyList()), emptyList())
            )
        } else {
            emptyList()
        }
    }

    private val db = ObjectDeclaration(
        name = "db",
        value = FunctionCall(
            Function(
                name = dbName, parameters = listOf(driver.declaration), returnType = DB
            ) {}, parameters = listOf(driver), comments = emptyList()
        ),
        private = false,
        mutable = false,
        comments = emptyList(),
        type = DB
    )

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
        val first = files == null
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

        val query = Use(
            db, FunctionCall(function, params.map { it.variable() }, emptyList()), emptyList()
        )
        val call = when (sql.type) {
            Select ->
                Use(
                    query, FunctionCall(
                        function = Function(name = "executeAsOne", parameters = emptyList()) {},
                        parameters = emptyList(),
                        comments = emptyList()
                    ), comments = sql.comments
                )

            Delete, Execute, Insert -> query.copy(comments = sql.comments)
        }

        return if (sql.hostVariables.isEmpty()) {
            if (first) listOf(db, call) else listOf(call)
        } else {
            val result = Class(
                name = queryName,
                constructor = Class.Constructor(emptyList()),
                functions = emptyList(),
                members = sql.hostVariables.map {
                    when (val result = variableToIR(it).target) {
                        is BooleanDeclaration, is ObjectDeclaration -> error("Not yet supported")
                        is DoubleDeclaration, is IntDeclaration, is StringDeclaration -> result
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
                value = call
            )
            (if (first) listOf(db, callResult) else listOf(callResult)) + sql.hostVariables.map { hostVariable ->
                val obj = callResult.variable() as KobolIRTree.Expression.ObjectVariable
                val variable = callResult.type.members.single { it.name == hostVariable.target.name }.variable()
                val use = when (hostVariable) {
                    is CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberVariable -> {
                        when (hostVariable.target.formatter.numberType) {
                            WorkingStorage.Elementar.Formatter.NumberType.Int -> KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable.Use(
                                obj,
                                variable as KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable,
                                emptyList()
                            )

                            WorkingStorage.Elementar.Formatter.NumberType.Double -> KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable.Use(
                                obj,
                                variable as KobolIRTree.Expression.NumberExpression.DoubleExpression.DoubleVariable,
                                emptyList()
                            )
                        }
                    }
                    is CobolFIRTree.ProcedureTree.Expression.StringExpression.StringVariable -> {
                        KobolIRTree.Expression.StringExpression.StringVariable.Use(
                            obj,
                            variable as KobolIRTree.Expression.StringExpression.StringVariable,
                            emptyList()
                        )
                    }
                }
                Assignment(
                    comments = emptyList(),
                    newValue = use,
                    declaration = getDeclaration(hostVariable.target)
                )
            }
        }
    }

    override fun close() {
        files?.writeTo(sqFolder)
    }
}
