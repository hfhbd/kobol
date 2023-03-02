package app.softwork.kobol.ir

import app.softwork.kobol.*
import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.ForEach
import app.softwork.kobol.ir.KobolIRTree.Types.Type.*

public class KotlinxSerialization(
    private val packageName: String
) : SerializationPlugin {

    override fun fileSection(fileSection: CobolFIRTree.DataTree.File): List<Class> {
        val records = fileSection.records
        return if (records.size == 1) {
            val ir = records.single().toIR(packageName)
            val ebcdicAnno = Class(
                "Ebcdic",
                "app.softwork.serialization.flf"
            )
            val signedAnnotation: Map<String, List<KobolIRTree.Expression>> = mapOf(
                "app.softwork.serialization.flf.Ebcdic" to listOf(
                    Static(
                        Class(
                            name = "Format",
                            packageName = "app.softwork.serialization.flf.Ebcdic"
                        )
                    ).use(
                        ObjectDeclaration("Zoned", type = Class(
                            name = "Format.Zoned",
                            packageName = "app.softwork.serialization.flf.Ebcdic"
                        ), value = null, nullable = false)
                    )
                )
            )
            val members = ir.members.map {
                when (it) {
                    is ObjectDeclaration -> it
                    is BooleanDeclaration -> it.copy(
                        annotations = mapOf(
                            "app.softwork.serialization.flf.FixedLength" to listOf(it.length.l)
                        ),
                        nullable = false
                    )

                    is DoubleDeclaration -> {
                        val signed = if (it.isSigned) {
                            signedAnnotation
                        } else emptyMap()
                        it.copy(
                            annotations = signed + mapOf(
                                "app.softwork.serialization.flf.FixedLength" to listOf(it.length.l)
                            ),
                            nullable = false
                        )
                    }

                    is IntDeclaration -> {
                        val signed = if (it.isSigned) {
                            signedAnnotation
                        } else emptyMap()
                        it.copy(
                            annotations = signed + mapOf(
                                "app.softwork.serialization.flf.FixedLength" to listOf(it.length.l)
                            ),
                            nullable = false
                        )
                    }

                    is StringDeclaration -> it.copy(
                        annotations = mapOf(
                            "app.softwork.serialization.flf.FixedLength" to listOf(it.length.l)
                        ),
                        nullable = false
                    )
                }
            }

            val inner = Class(
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

    private val format = Class(
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
        val klass = Class(
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
            type = Class(name = "BufferedReader", packageName = "java.io"),
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

    private fun Class.serializer() = Use(
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
            type = Class(name = "BufferedWriter", packageName = "java.io"),
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
        val klass = Class(
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
