@file:UseSerializers(NothingSerializer::class)

package app.softwork.kobol.fir

import app.softwork.kobol.*
import app.softwork.kobol.fir.CobolFIRTree.DataTree.WorkingStorage.Elementar.NumberElementar
import app.softwork.kobol.fir.CobolFIRTree.DataTree.WorkingStorage.Elementar.StringElementar
import app.softwork.kobol.fir.serializer.*
import kotlinx.serialization.*

/**
 * Never change the [fileName]
 */
@Serializable
public data class CobolFIRTree(
    val fileName: String,
    val packageName: String? = null,
    val fileComments: List<String> = emptyList(),
    val id: ID,
    val env: EnvTree? = null,
    val data: DataTree = returnCode,
    val procedure: ProcedureTree,
) {
    @Serializable
    public data class ID(
        val programID: String,
        val programIDComments: List<String> = emptyList(),
        val author: String? = null,
        val authorComments: List<String> = emptyList(),
        val installation: String? = null,
        val installationComments: List<String> = emptyList(),
        val date: String? = null,
        val dateComments: List<String> = emptyList(),
    )

    @Serializable
    public data class EnvTree(
        val configuration: Configuration? = null,
        val inputOutput: InputOutput? = null,
        val comments: List<String> = emptyList(),
    ) {
        @Serializable
        public data class Configuration(
            val specialNames: SpecialNames? = null,
            val comments: List<String> = emptyList(),
        ) {
            @Serializable
            public data class SpecialNames(
                val specialNames: List<SpecialName>,
                val comments: List<String> = emptyList(),
            ) {
                @Serializable
                public data class SpecialName(
                    val env: String,
                    val value: String,
                    val comments: List<String> = emptyList(),
                )
            }
        }

        @Serializable
        public data class InputOutput(
            val fileControl: FileControl? = null,
            val comments: List<String> = emptyList(),
        ) {
            @Serializable
            public data class FileControl(
                val files: List<File> = emptyList(),
                val comments: List<String> = emptyList(),
            ) {
                // TODO: Use only one file class
                @Serializable
                public data class File(
                    val file: String,
                    val path: String,
                    val fileStatus: String?,
                    val type: DataTree.File.FileType,
                    val comments: List<String> = emptyList(),
                )
            }
        }
    }

    @Serializable
    public data class DataTree(
        val fileSection: List<File> = emptyList(),
        val sql: List<Sql> = emptyList(),
        val workingStorage: List<WorkingStorage> = emptyList(),
        val linkingSection: List<WorkingStorage> = emptyList(),
        val comments: List<String> = emptyList(),
    ) {
        @Serializable
        public data class Sql(
            val sql: String,
            val comments: List<String> = emptyList(),
        )

        @Serializable
        public data class File(
            val name: String,
            val description: FileDescription,
            val records: List<WorkingStorage.Record>,
            val fileStatus: String?,
            val filePath: String,
            val type: FileType,
        ) {
            @Serializable
            public enum class FileType {
                Sequential, LineSequential
            }

            val recordName: String = records.map { it.name }.let {
                val name = it.distinct()
                name.singleOrNull() ?: error("Different Record names found for file $name: $it")
            }

            @Serializable
            public data class FileDescription(
                val recording: String? = null,
                val blocks: Int? = null,
                @Serializable(with = IntRangeSerializer::class) val records: IntRange? = null,
                val comments: List<String> = emptyList(),
            )
        }

        @Serializable
        public sealed interface WorkingStorage {
            public val comments: List<String>

            @Serializable
            public data class Record(
                val name: String,
                val elements: List<Elementar> = emptyList(),
                override val comments: List<String> = emptyList(),
            ) : WorkingStorage {
                public constructor(
                    name: String,
                    comments: List<String> = emptyList(),
                    elements: Builder<Elementar>.() -> Unit,
                ) : this(name, build(elements), comments)
            }

            @Serializable
            public sealed interface Elementar : WorkingStorage {
                public val name: String
                public val recordName: String?
                public val formatter: Formatter?
                public val value: Any?

                @Serializable
                public data class Pointer(
                    override val name: String,
                    override val recordName: String?,
                    override val comments: List<String> = emptyList(),
                ) : Elementar {
                    override val formatter: Nothing? = null
                    override val value: Nothing? = null
                }

                @Serializable
                public data class StringElementar(
                    override val name: String,
                    override val recordName: String?,
                    override val formatter: Formatter,
                    override val value: String? = null,
                    val occurs: Occurs? = null,
                    val synthetic: Boolean = false,
                    override val comments: List<String> = emptyList(),
                ) : Elementar {
                    init {
                        if (value != null) {
                            require(!value.startsWith("\""))
                            require(!value.startsWith("'"))
                        }
                    }
                }

                @Serializable
                public data class Occurs(
                    val from: Int,
                    val to: Int? = null,
                    val dependingOn: NumberElementar? = null,
                )

                @Serializable
                public sealed interface Formatter {

                    public fun length(): Int = when (this) {
                        is Simple -> length
                        is Custom -> parts.sumOf { it.length }
                    }

                    public val isSigned: Boolean
                        get() {
                            return when (this) {
                                is Simple -> false
                                is Custom -> {
                                    for (part in parts) {
                                        when (part) {
                                            is Custom.Part.Signed -> return true

                                            else -> continue
                                        }
                                    }
                                    false
                                }
                            }
                        }

                    public val numberType: NumberType
                        get() {
                            return when (this) {
                                is Simple -> NumberType.Int
                                is Custom -> {
                                    for (part in parts) {
                                        when (part) {
                                            is Custom.Part.Decimal -> return NumberType.Double
                                            is Custom.Part.Number,
                                            is Custom.Part.Plus,
                                            is Custom.Part.Signed,
                                            -> continue

                                            is Custom.Part.Space,
                                            is Custom.Part.String,
                                            is Custom.Part.Zero,
                                            -> error("Not supported")
                                        }
                                    }
                                    NumberType.Int
                                }
                            }
                        }

                    @Serializable
                    public enum class NumberType {
                        Int, Double
                    }

                    @Serializable
                    @JvmInline
                    public value class Simple(public val length: Int) : Formatter

                    @Serializable
                    public data class Custom(val parts: List<Part>) : Formatter {
                        public constructor(vararg parts: Part) : this(parts = parts.toList())

                        @Serializable
                        public sealed interface Part {
                            public val length: Int

                            @Serializable
                            @JvmInline
                            public value class Signed(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            public value class Number(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            public value class String(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            public value class Space(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            public value class Zero(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            public value class Plus(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            public value class Decimal(override val length: Int) : Part
                        }
                    }
                }

                @Serializable
                public sealed interface NumberElementar : Elementar {
                    override val value: Number?
                    override val formatter: Formatter
                    public val occurs: Occurs?
                    public val signed: Boolean
                    public val compressed: Compressed?
                    public val binary: Boolean
                    public val synthetic: Boolean

                    @Serializable
                    public data class Normal(
                        override val name: String,
                        override val recordName: String?,
                        override val formatter: Formatter,
                        override val value: Double? = null,
                        override val occurs: Occurs? = null,
                        override val comments: List<String> = emptyList(),
                        override val signed: Boolean = false,
                        override val compressed: Compressed? = null,
                        override val binary: Boolean = false,
                        override val synthetic: Boolean = false,
                    ) : NumberElementar

                    // https://www.ibm.com/docs/en/cobol-zos/6.3?topic=registers-return-code
                    @Serializable
                    public data class ReturnCode(
                        override val name: String = "RETURN-CODE",
                    ) : NumberElementar {
                        override val value: Int = 0
                        override val occurs: Occurs? = null
                        override val signed: Boolean = true
                        override val compressed: Compressed? = null
                        override val binary: Boolean = false

                        override val formatter: Formatter = Formatter.Simple(4)
                        override val comments: List<String> = emptyList()
                        override val recordName: String? = null
                        override val synthetic: Boolean = true
                    }

                    @Serializable
                    public enum class Compressed {
                        COMP, COMP3, COMP5
                    }
                }

                @Serializable
                public data class EmptyElementar(
                    override val name: String,
                    override val recordName: String?,
                    override val comments: List<String> = emptyList(),
                ) : Elementar {
                    override val formatter: Nothing? = null
                    override val value: Nothing? = null
                }
            }
        }
    }

    @Serializable
    public data class ProcedureTree(
        val topLevel: List<Statement> = emptyList(),
        val sections: List<Section> = emptyList(),
        val comments: List<String> = emptyList(),
    ) {

        @Serializable
        public data class Section(
            val name: String,
            val statements: List<Statement> = emptyList(),
            val comments: List<String> = emptyList(),
        ) {
            public constructor(
                name: String,
                comments: List<String> = emptyList(),
                builder: Builder<Statement>.() -> Unit,
            ) : this(name, build(builder), comments)
        }

        @Serializable
        public sealed interface Statement {
            public val comments: List<String>

            @Serializable
            public data class Move(
                val target: DataTree.WorkingStorage.Elementar,
                val value: Expression,
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class Add(
                val target: DataTree.WorkingStorage.Elementar,
                val value: Expression,
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class Sub(
                val target: DataTree.WorkingStorage.Elementar,
                val value: Expression,
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class Display(
                val expr: Expression.StringExpression,
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class Perform(
                val sectionName: String,
                override val comments: List<String> = emptyList(),
                val until: Expression.BooleanExpression? = null,
                val testing: Testing = Testing.Before,
            ) : Statement {
                @Serializable
                public enum class Testing {
                    After,
                    Before,
                }
            }

            @Serializable
            public data class While(
                val statements: List<Statement>,
                override val comments: List<String> = emptyList(),
                val until: Expression.BooleanExpression,
            ) : Statement

            @Serializable
            public data class ForEach(
                val variable: NumberElementar,
                val from: Expression.NumberExpression,
                val by: Expression.NumberExpression? = null,
                val until: Expression.BooleanExpression,
                val statements: List<Statement>,
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class If(
                val condition: Expression.BooleanExpression,
                val statements: List<Statement>,
                val elseStatements: List<Statement> = emptyList(),
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class Eval(
                val values: List<Expression>,
                val conditions: List<Condition>,
                val other: Other?,
                override val comments: List<String> = emptyList(),
            ) : Statement {
                @Serializable
                public data class Condition(
                    val conditions: List<Expression>,
                    val action: List<Statement>,
                    val comments: List<String> = emptyList(),
                )

                @Serializable
                public data class Other(
                    val action: List<Statement>,
                    val comments: List<String> = emptyList(),
                )
            }

            @Serializable
            public data class GoBack(
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class StopRun(
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class Continue(
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class Call(
                val name: String,
                val parameters: List<Expression> = emptyList(),
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class Sql(
                val sql: String,
                val updatingHostVariables: List<Expression.Variable>,
                val parameter: List<Expression.Variable>,
                override val comments: List<String> = emptyList(),
                val type: SqlType,
            ) : Statement {
                @Serializable
                public enum class SqlType {
                    Select, Insert, Delete, Execute
                }
            }

            @Serializable
            public data class Read(
                val file: DataTree.File,
                val action: List<Statement>,
                val atEnd: List<Statement>,
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class Write(
                val file: DataTree.File,
                val from: DataTree.WorkingStorage?,
                override val comments: List<String> = emptyList(),
            ) : Statement

            @Serializable
            public data class Open(
                val file: DataTree.File,
                val type: Type,
                override val comments: List<String> = emptyList(),
            ) : Statement {
                @Serializable
                public enum class Type {
                    Input, Output
                }
            }

            @Serializable
            public data class Close(
                val files: List<DataTree.File>,
                override val comments: List<String> = emptyList(),
            ) : Statement
        }

        @Serializable
        public sealed interface Expression {
            @Serializable
            public sealed interface BooleanExpression : Expression {
                @Serializable
                public data class Equals(val left: Expression, val right: Expression) :
                    BooleanExpression

                @Serializable
                public data class Not(val target: BooleanExpression) : BooleanExpression

                @Serializable
                public data class Or(val left: BooleanExpression, val right: BooleanExpression) :
                    BooleanExpression

                @Serializable
                public data class And(val left: BooleanExpression, val right: BooleanExpression) :
                    BooleanExpression

                @Serializable
                public data class Greater(
                    val left: NumberExpression,
                    val right: NumberExpression,
                    val equals: Boolean = false,
                ) : BooleanExpression

                @Serializable
                public data class Smaller(
                    val left: NumberExpression,
                    val right: NumberExpression,
                    val equals: Boolean = false,
                ) : BooleanExpression
            }

            @Serializable
            public sealed interface Literal : Expression {
                public val value: Any
            }

            @Serializable
            public sealed interface Variable : Expression {
                public val target: DataTree.WorkingStorage.Elementar
            }

            @Serializable
            public sealed interface StringExpression : Expression {
                public operator fun plus(right: Expression): Concat = Concat(this, right)

                @Serializable
                public data class StringLiteral(override val value: String) :
                    StringExpression,
                    Literal {
                    init {
                        require(!value.startsWith("\""))
                        require(!value.startsWith("'"))
                    }
                }

                @Serializable
                public data class StringVariable(
                    override val target: StringElementar,
                ) : StringExpression, Variable

                @Serializable
                public data class Concat(val left: Expression, val right: Expression) :
                    StringExpression

                @Serializable
                public data class Interpolation(val value: Expression) : StringExpression
            }

            @Serializable
            public sealed interface NumberExpression : Expression {
                @Serializable
                public data class NumberLiteral(
                    override val value: Double,
                ) : NumberExpression, Literal

                @Serializable
                public data class NumberVariable(
                    override val target: NumberElementar,
                ) : Variable, NumberExpression
            }
        }
    }
}
