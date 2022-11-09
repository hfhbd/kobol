package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.KobolIRTree.Expression.StringExpression.*
import app.softwork.kobol.KobolIRTree.Types.Type.*

object JavaFilesKotlin : FileHandling {
    private val BufferedReader = Class(
        name = "BufferedReader",
        packageName = "java.io"
    )

    private val BufferedWriter = Class(
        name = "BufferedWriter",
        packageName = "java.io"
    )

    private val Closeable = Class(
        name = "Closeable",
        packageName = "java.io"
    )

    private val bufferedReader = KobolIRTree.Types.Function(
        name = "bufferedReader",
        returnType = BufferedReader,
    ) { }
    private val bufferedWriter = KobolIRTree.Types.Function(
        name = "bufferedWriter",
        returnType = BufferedWriter
    ) { }

    private val files = Class(
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

    override fun handleOpen(open: Open): List<KobolIRTree.Types.Function.Statement> {
        val file = open.file

        val (type, call) = when (open.type) {
            Open.Type.Input -> BufferedReader to bufferedReader
            Open.Type.Output -> BufferedWriter to bufferedWriter
        }

        return listOf(
            KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration(
                name = file.name,
                type = type,
                value = KobolIRTree.Types.Function.Statement.Use(
                    KobolIRTree.Types.Function.Statement.FunctionCall(
                        files,
                        listOf(StringLiteral(file.name)),
                        comments = open.comments
                    ),
                    KobolIRTree.Types.Function.Statement.FunctionCall(
                        call,
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

    override fun handleClose(close: Close) = listOf(
        KobolIRTree.Types.Function.Statement.Use(
            target = KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration(
                name = close.file.name,
                type = Closeable,
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
