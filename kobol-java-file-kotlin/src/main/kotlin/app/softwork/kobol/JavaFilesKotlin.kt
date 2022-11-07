package app.softwork.kobol

import app.softwork.kobol.KobolIRTree.Expression.StringExpression.*
import app.softwork.kobol.KobolIRTree.Types.Type.*

object JavaFilesKotlin : FileHandling {
    override fun handleOpen(open: CobolFIRTree.ProcedureTree.Statement.Open): List<KobolIRTree.Types.Function.Statement> {
        val file = open.file

        val bufferedReader = KobolIRTree.Types.Function(
            name = "bufferedReader",
            returnType = BufferedReader,
        ) { }
        val bufferedWriter = KobolIRTree.Types.Function(
            name = "bufferedWriter",
            returnType = BufferedWriter
        ) { }

        val files = Class(
            name = "File",
            packageName = "java.io",
            functions = listOf(bufferedReader, bufferedWriter),
            constructor = listOf(
                KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration(
                    name = "",
                    value = null,
                    nullable = false,
                    mutable = false,
                    private = false,
                    const = false,
                    length = -1
                )
            )
        )

        return listOf(
            KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration(
                name = file,
                type = BufferedReader,
                value = KobolIRTree.Types.Function.Statement.Use(
                    KobolIRTree.Types.Function.Statement.FunctionCall(
                        files,
                        listOf(StringLiteral(file)),
                        comments = open.comments
                    ),
                    KobolIRTree.Types.Function.Statement.FunctionCall(
                        bufferedReader,
                        parameters = listOf(
                            KobolIRTree.Types.Function.Statement.FunctionCall(
                                KobolIRTree.Types.Function(
                                    name = "charset",
                                    packageName = "kotlin.text",
                                    topLevel = true,
                                ) {},
                                listOf(StringLiteral("IBM-1047"))
                            )
                        ),
                    )
                )
            )
        )
    }

    private val BufferedReader = Class(
        name = "BufferedReader",
        packageName = "java.io"
    )

    private val BufferedWriter = Class(
        name = "BufferedWriter",
        packageName = "java.io"
    )

    override fun handleClose(close: CobolFIRTree.ProcedureTree.Statement.Close) = listOf(
        KobolIRTree.Types.Function.Statement.Use(
            target = KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration(
                name = close.file,
                type = BufferedReader, // TODO: use bufferedWriter too
                value = null,
                nullable = false,
            ),
            action = KobolIRTree.Types.Function.Statement.FunctionCall(
                KobolIRTree.Types.Function(
                    name = "close"
                ) {},
                parameters = emptyList(),
            )
        )
    )
}
