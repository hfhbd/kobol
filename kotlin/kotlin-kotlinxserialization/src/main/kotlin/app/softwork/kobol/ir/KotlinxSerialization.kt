package app.softwork.kobol.ir

import app.softwork.kobol.*
import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.ForEach

public class KotlinxSerialization(
    private val packageName: String
) : SerializationPlugin {

    override fun fileSection(fileSection: CobolFIRTree.DataTree.File): List<KobolIRTree.Types.Type.Class> {
        val records = fileSection.records
        return if (records.size == 1) {
            val ir = records.single().toIR(packageName)
            val members = ir.members.map {
                when (it) {
                    is ObjectDeclaration -> it
                    is BooleanDeclaration -> it.copy(
                        annotations = mapOf(
                            "app.softwork.serialization.flf.FixedLength" to listOf(it.length.toString())
                        ),
                        nullable = false
                    )

                    is DoubleDeclaration -> it.copy(
                        annotations = mapOf(
                            "app.softwork.serialization.flf.FixedLength" to listOf(it.length.toString())
                        ),
                        nullable = false
                    )

                    is IntDeclaration -> it.copy(
                        annotations = mapOf(
                            "app.softwork.serialization.flf.FixedLength" to listOf(it.length.toString())
                        ),
                        nullable = false
                    )

                    is StringDeclaration -> it.copy(
                        annotations = mapOf(
                            "app.softwork.serialization.flf.FixedLength" to listOf(it.length.toString())
                        ),
                        nullable = false
                    )
                }
            }

            val inner = KobolIRTree.Types.Type.Class(
                "companion",
                packageName = packageName,
                isObject = true,
                members = ir.members,
                functions = listOf(
                    KobolIRTree.Types.Function(
                        name = "create",
                        returnType = ir
                    ) {
                        +Return(
                            FunctionCall(
                                ir,
                                ir.members.map { it.variable() }
                            ),
                        )
                    })
            )

            listOf(
                ir.copy(
                    isData = true,
                    isObject = false,
                    constructor = members,
                    members = members,
                    annotations = mapOf(
                        "kotlinx.serialization.ExperimentalSerializationApi" to emptyList(),
                        "kotlinx.serialization.Serializable" to emptyList()
                    ),
                    inner = listOf(inner)
                )
            )
        } else TODO("Sealed class")
    }

    private val decodeAsSequence by function { }

    private val format = KobolIRTree.Types.Type.Class(
        "FixedLengthFormat",
        "app.softwork.serialization.flf",
        isObject = true,
        isData = false,
        functions = listOf(decodeAsSequence)
    )
    private val formatObject = ObjectDeclaration(
        name = "FixedLengthFormat",
        type = format,
        comments = emptyList(),
        mutable = false,
        private = false,
        value = null,
        static = true
    )

    override fun readSequence(
        read: Read,
        toIR: List<CobolFIRTree.ProcedureTree.Statement>.() -> List<Statement>
    ): List<Statement> {
        val dataRecord = read.file.recordName
        val klass = KobolIRTree.Types.Type.Class(
            packageName = packageName,
            name = dataRecord,
            isObject = true,
            isData = false,
        )

        val variable = ObjectDeclaration(
            type = klass,
            name = dataRecord,
            mutable = false,
            private = false,
            value = null,
            nullable = false
        )

        val readBufferedReader = ObjectDeclaration(
            type = KobolIRTree.Types.Type.Class(name = "BufferedReader", packageName = "java.io"),
            name = read.file.name,
            value = null,
            nullable = false
        )

        return build {
            +ForEach(
                variable = variable,
                provider = Use(
                    target = readBufferedReader,
                    action = Use(
                        target = FunctionCall(
                            KobolIRTree.Types.Function(
                                "lineSequence"
                            ) {},
                            parameters = emptyList()
                        ),
                        action = FunctionCall(
                            KobolIRTree.Types.Function(
                                "decode",
                                topLevel = true,
                                packageName = "app.softwork.serialization.flf"
                            ) {},
                            parameters = listOf(klass.serializer())
                        ),
                    )
                ),
                statements = read.action.toIR(),
                comments = read.comments
            )
        } + read.atEnd.toIR()
    }

    private fun KobolIRTree.Types.Type.Class.serializer() = Use(
        target = Static(this),
        action = FunctionCall(
            function = KobolIRTree.Types.Function(
                name = "serializer",
                returnType = KobolIRTree.Types.Type.Void,
            ) {},
            parameters = emptyList()
        ),
    )

    override fun write(write: Write): List<Statement> = build {
        val readBufferedWriter = ObjectDeclaration(
            type = KobolIRTree.Types.Type.Class(name = "BufferedWriter", packageName = "java.io"),
            name = write.file.name,
            value = null,
            nullable = false
        )
        val append = KobolIRTree.Types.Function(
            name = "append",
            topLevel = true,
            packageName = "app.softwork.serialization.flf"
        ) {}

        val dataRecord = write.file.recordName
        val klass = KobolIRTree.Types.Type.Class(
            packageName = packageName,
            name = dataRecord,
            isObject = true,
            isData = false,
        )

        +(readBufferedWriter use append.call(listOf(
            klass.serializer(),
            (Static(klass) use (function("create") {} call emptyList())))
        ))
    }
}
