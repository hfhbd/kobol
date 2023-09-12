package app.softwork.kobol.ir

import app.softwork.kobol.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Expression.StringExpression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.serviceloader.*

@ServiceLoader(FileHandlingFactory::class)
public class JavaFilesKotlin : FileHandling, FileHandlingFactory {
    override fun invoke(packageName: String, args: Map<String, String>): JavaFilesKotlin = JavaFilesKotlin()

    private val BufferedReader by klass(
        packageName = "java.io",
    )

    private val BufferedWriter by klass(
        packageName = "java.io",
    )

    private val Closeable by klass(
        packageName = "java.io",
    )

    private val bufferedReader by function(
        returnType = BufferedReader,
    ) { }

    private val bufferedWriter by function(
        returnType = BufferedWriter,
    ) { }

    private val File by klass(
        packageName = "java.io",
        functions = {
            +bufferedReader
            +bufferedWriter
        },
        constructor = {
            +Declaration.StringDeclaration(
                name = "",
                value = null,
                nullable = false,
                mutable = false,
                private = false,
                const = false,
                length = -1,
            )
        },
    )

    override fun handleOpen(open: Open): List<KobolIRTree.Types.Function.Statement> {
        val file = open.file

        val (type, call) = when (open.type) {
            Open.Type.Input -> BufferedReader to bufferedReader
            Open.Type.Output -> BufferedWriter to bufferedWriter
        }
        val charset by function(
            packageName = "kotlin.text",
            topLevel = true,
        ) {}

        return listOf(
            Declaration.ObjectDeclaration(
                name = file.name,
                type = type,
                value = FunctionCall(
                    function = File,
                    parameters = listOf(StringLiteral(file.name)),
                    comments = open.comments,
                ) use call(
                    charset(StringLiteral("IBM-1140")),
                ),
            ),
        )
    }

    override fun handleClose(close: Close): List<Use> = close.files.map {
        Declaration.ObjectDeclaration(
            name = it.name,
            type = Closeable,
            value = null,
            nullable = false,
        ) use FunctionCall(
            KobolIRTree.Types.Function(
                name = "close",
            ) {},
            parameters = emptyList(),
        )
    }
}
