package app.softwork.kobol

import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*

class KotlinxSerialization(
    private val packageName: String
) : SerializationPlugin {
    override fun fileSection(fileSection: CobolFIRTree.DataTree.FileSection): List<KobolIRTree.Types.Type.Class> {
        val records = fileSection.records
        return if (records.size == 1) {
            val ir = records.single().toIR(packageName)
            val members = ir.members.map {
                when (it) {
                    is KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration -> it
                    is KobolIRTree.Types.Function.Statement.Declaration.BooleanDeclaration -> it.copy(
                        annotations = mapOf(
                            "app.softwork.serialization.flf.FixedLength" to listOf(it.length.toString())
                        ),
                        nullable = false
                    )

                    is KobolIRTree.Types.Function.Statement.Declaration.DoubleDeclaration -> it.copy(
                        annotations = mapOf(
                            "app.softwork.serialization.flf.FixedLength" to listOf(it.length.toString())
                        ),
                        nullable = false
                    )

                    is KobolIRTree.Types.Function.Statement.Declaration.IntDeclaration -> it.copy(
                        annotations = mapOf(
                            "app.softwork.serialization.flf.FixedLength" to listOf(it.length.toString())
                        ),
                        nullable = false
                    )

                    is KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration -> it.copy(
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
    val formatObject = KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration(
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
        val dataRecord = read.dataRecords.first().name
        val klass = KobolIRTree.Types.Type.Class(
            packageName = packageName,
            name = dataRecord,
            isObject = true,
            isData = false,
        )

        val variable = KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration(
            type = klass,
            name = dataRecord,
            mutable = false,
            private = false,
            value = null,
            nullable = false
        )

        val readBufferedReader = KobolIRTree.Types.Function.Statement.Declaration.ObjectDeclaration(
            type = KobolIRTree.Types.Type.Class(name = "BufferedReader", packageName = "java.io"),
            name = read.file,
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
}
