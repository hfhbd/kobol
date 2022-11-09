package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.KobolIRTree.Types.Function.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.ForEach

class KotlinxSerialization(
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
            listOf(
                ir.copy(
                    isData = true,
                    isObject = false,
                    constructor = members,
                    members = members,
                    annotations = mapOf(
                        "kotlinx.serialization.ExperimentalSerializationApi" to emptyList(),
                        "kotlinx.serialization.Serializable" to emptyList()
                    )
                )
            )
        } else TODO("Sealed class")
    }

    val decodeAsSequence by function { }

    val format = KobolIRTree.Types.Type.Class(
        "FixedLengthFormat",
        "app.softwork.serialization.flf",
        isObject = true,
        isData = false,
        functions = listOf(decodeAsSequence)
    )
    val formatObject = ObjectDeclaration(
        name = "FixedLengthFormat",
        type = format,
        comments = emptyList(),
        mutable = false,
        private = false,
        value = null,
        static = true
    )

    override fun readSequence(
        read: CobolFIRTree.ProcedureTree.Statement.Read,
        toIR: List<CobolFIRTree.ProcedureTree.Statement>.() -> List<KobolIRTree.Types.Function.Statement>
    ): List<KobolIRTree.Types.Function.Statement> {
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
                            KobolIRTree.Types.Function("decode") {},
                            parameters = listOf(
                                Use(
                                    target = Static(klass),
                                    action = FunctionCall(
                                        function = KobolIRTree.Types.Function(
                                            name = "serializer",
                                            returnType = KobolIRTree.Types.Type.Void,
                                        ) {},
                                        parameters = emptyList()
                                    ),
                                )
                            ),
                        ),
                    )
                ),
                statements = read.action.toIR(),
                comments = read.comments
            )

            +read.atEnd.toIR()
        }
    }

    override fun write(write: Write): List<Statement> {
        return emptyList()
    }
}
