@file:UseSerializers(NothingSerializer::class)

package app.softwork.kobol

import app.softwork.kobol.serializer.*
import kotlinx.serialization.*

@Serializable
data class CobolFIRTree(
    val fileComments: List<String> = emptyList(),
    val id: ID,
    val env: EnvTree? = null,
    val data: DataTree? = null,
    val procedure: ProcedureTree
) {
    @Serializable
    data class ID(
        val programID: String,
        val programIDComments: List<String> = emptyList(),
        val author: String? = null,
        val authorComments: List<String> = emptyList(),
        val installation: String? = null,
        val installationsComments: List<String> = emptyList(),
        val date: String? = null,
        val dateComments: List<String> = emptyList()
    )

    @Serializable
    data class EnvTree(
        val configuration: Configuration? = null,
        val inputOutput: InputOutput? = null,
        val comments: List<String> = emptyList()
    ) {
        @Serializable
        data class Configuration(
            val specialNames: SpecialNames? = null, val comments: List<String> = emptyList()
        ) {
            @Serializable
            data class SpecialNames(
                val specialNames: List<SpecialName>, val comments: List<String> = emptyList()
            ) {
                @Serializable
                data class SpecialName(val env: String, val value: String, val comments: List<String> = emptyList())
            }
        }

        @Serializable
        data class InputOutput(
            val fileControl: FileControl? = null, val comments: List<String> = emptyList()
        ) {
            @Serializable
            data class FileControl(
                val files: List<File> = emptyList(), val comments: List<String> = emptyList()
            ) {
                @Serializable
                data class File(
                    val file: String,
                    val fileVariable: String,
                    val fileStatus: String,
                    val comments: List<String> = emptyList()
                )
            }
        }
    }

    @Serializable
    data class DataTree(
        val fileSection: FileSection? = null,
        val workingStorage: List<WorkingStorage> = emptyList(),
        val linkingSection: List<WorkingStorage> = emptyList(),
        val comments: List<String> = emptyList()
    ) {
        @Serializable
        data class FileSection(
            val descriptions: List<FileDescription> = emptyList(), val comments: List<String> = emptyList()
        ) {
            @Serializable
            sealed interface FileDescription {
                val comments: List<String>

                @Serializable
                data class FileDefinition(
                    val name: String,
                    val recording: String,
                    val blocks: Int? = null,
                    @Serializable(with = IntRangeSerializer::class) val records: IntRange? = null,
                    val dataRecord: String? = null,
                    override val comments: List<String> = emptyList()
                ) : FileDescription
            }
        }

        @Serializable
        sealed interface WorkingStorage {
            val comments: List<String>

            @Serializable
            data class Record(
                val name: String,
                val elements: List<Elementar> = emptyList(),
                override val comments: List<String> = emptyList()
            ) : WorkingStorage, FileSection.FileDescription {
                constructor(name: String, elements: Builder<Elementar>.() -> Unit) : this(name, build(elements))
            }

            @Serializable
            sealed interface Elementar : WorkingStorage {
                val name: String
                val recordName: String?
                val formatter: Formatter?
                val value: Any?

                @Serializable
                data class Pointer(
                    override val name: String,
                    override val recordName: String?,
                    override val comments: List<String> = emptyList()
                ) : Elementar {
                    override val formatter: Nothing? = null
                    override val value: Nothing? = null
                }

                @Serializable
                data class StringElementar(
                    override val name: String,
                    override val recordName: String?,
                    override val formatter: Formatter,
                    override val value: String? = null,
                    val occurs: Occurs? = null,
                    override val comments: List<String> = emptyList()
                ) : Elementar {
                    init {
                        if (value != null) {
                            require(!value.startsWith("\""))
                            require(!value.startsWith("'"))
                        }
                    }
                }

                @Serializable
                data class Occurs(val from: Int, val to: Int? = null, val dependingOn: NumberElementar? = null)

                @Serializable
                sealed interface Formatter {

                    val numberType: NumberType
                        get() {
                            return when (this) {
                                is Simple -> NumberType.Int
                                is Custom -> {
                                    for (part in parts) {
                                        when (part) {
                                            is Custom.Part.Decimal -> return NumberType.Double
                                            is Custom.Part.Number,
                                            is Custom.Part.Plus,
                                            is Custom.Part.Signed -> continue

                                            is Custom.Part.Space,
                                            is Custom.Part.String,
                                            is Custom.Part.Zero -> error("Not supported")
                                        }
                                    }
                                    NumberType.Int
                                }
                            }
                        }

                    enum class NumberType {
                        Int, Double
                    }

                    @Serializable
                    @JvmInline
                    value class Simple(val length: Int) : Formatter

                    @Serializable
                    data class Custom(val parts: List<Part>) : Formatter {
                        constructor(vararg parts: Part) : this(parts = parts.toList())

                        @Serializable
                        sealed interface Part {
                            val length: Int

                            @Serializable
                            @JvmInline
                            value class Signed(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            value class Number(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            value class String(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            value class Space(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            value class Zero(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            value class Plus(override val length: Int) : Part

                            @Serializable
                            @JvmInline
                            value class Decimal(override val length: Int) : Part
                        }
                    }
                }

                @Serializable
                data class NumberElementar(
                    override val name: String,
                    override val recordName: String?,
                    override val formatter: Formatter,
                    override val value: Double? = null,
                    val occurs: Occurs? = null,
                    override val comments: List<String> = emptyList(),
                    val signed: Boolean = false,
                    val compressed: Compressed? = null,
                    val binary: Boolean = false
                ) : Elementar {
                    @Serializable
                    enum class Compressed {
                        COMP, COMP3, COMP5
                    }
                }

                @Serializable
                data class EmptyElementar(
                    override val name: String,
                    override val recordName: String?,
                    override val comments: List<String> = emptyList()
                ) : Elementar {
                    override val formatter: Nothing? = null
                    override val value: Nothing? = null
                }
            }
        }
    }

    @Serializable
    data class ProcedureTree(
        val topLevel: List<Statement> = emptyList(),
        val sections: List<Section> = emptyList(),
        val comments: List<String> = emptyList()
    ) {

        @Serializable
        data class Section(
            val name: String, val statements: List<Statement> = emptyList(), val comments: List<String> = emptyList()
        ) {
            constructor(
                name: String, comments: List<String> = emptyList(), builder: Builder<Statement>.() -> Unit
            ) : this(name, build(builder), comments)
        }

        @Serializable
        sealed interface Statement {
            val comments: List<String>

            @Serializable
            data class Move(
                val target: DataTree.WorkingStorage.Elementar,
                val value: Expression,
                override val comments: List<String> = emptyList()
            ) : Statement

            @Serializable
            data class Display(
                val expr: Expression.StringExpression, override val comments: List<String> = emptyList()
            ) : Statement

            @Serializable
            data class Perform(
                val sectionName: String,
                override val comments: List<String> = emptyList(),
                val until: Expression.BooleanExpression? = null
            ) : Statement

            @Serializable
            data class While(
                val statements: List<Statement>,
                override val comments: List<String> = emptyList(),
                val until: Expression.BooleanExpression
            ) : Statement

            @Serializable
            data class ForEach(
                val variable: DataTree.WorkingStorage.Elementar.NumberElementar,
                val from: Expression.NumberExpression,
                val by: Expression.NumberExpression? = null,
                val until: Expression.BooleanExpression,
                val statements: List<Statement>,
                override val comments: List<String> = emptyList()
            ) : Statement

            @Serializable
            data class If(
                val condition: Expression.BooleanExpression,
                val statements: List<Statement>,
                val elseStatements: List<Statement> = emptyList(),
                override val comments: List<String> = emptyList()
            ) : Statement

            @Serializable
            data class Eval(
                val values: List<Expression>,
                val conditions: List<Condition>,
                val other: Other?,
                override val comments: List<String> = emptyList()
            ) : Statement {
                @Serializable
                data class Condition(
                    val conditions: List<Expression>,
                    val action: List<Statement>,
                    val comments: List<String> = emptyList()
                )

                @Serializable
                data class Other(
                    val action: List<Statement>,
                    val comments: List<String> = emptyList()
                )
            }

            @Serializable
            data class GoBack(
                override val comments: List<String> = emptyList()
            ) : Statement

            @Serializable
            data class Continue(
                override val comments: List<String> = emptyList()
            ) : Statement

            @Serializable
            data class Call(
                val name: String,
                val parameters: List<Expression> = emptyList(),
                override val comments: List<String> = emptyList()
            ) : Statement
        }

        @Serializable
        sealed interface Expression {
            @Serializable
            sealed interface BooleanExpression : Expression {
                @Serializable
                data class Equals(val left: Expression, val right: Expression) : BooleanExpression

                @Serializable
                data class Not(val target: BooleanExpression) : BooleanExpression

                @Serializable
                data class Or(val left: BooleanExpression, val right: BooleanExpression) : BooleanExpression

                @Serializable
                data class And(val left: BooleanExpression, val right: BooleanExpression) : BooleanExpression

                @Serializable
                data class Greater(
                    val left: NumberExpression,
                    val right: NumberExpression,
                    val equals: Boolean = false
                ) : BooleanExpression

                @Serializable
                data class Smaller(
                    val left: NumberExpression,
                    val right: NumberExpression,
                    val equals: Boolean = false
                ) : BooleanExpression
            }

            @Serializable
            sealed interface Literal : Expression {
                val value: Any
            }

            @Serializable
            sealed interface Variable : Expression {
                val target: DataTree.WorkingStorage.Elementar
            }

            @Serializable
            sealed interface StringExpression : Expression {
                operator fun plus(right: Expression) = Concat(this, right)

                @Serializable
                data class StringLiteral(override val value: String) : StringExpression, Literal {
                    init {
                        require(!value.startsWith("\""))
                        require(!value.startsWith("'"))
                    }
                }

                @Serializable
                data class StringVariable(override val target: DataTree.WorkingStorage.Elementar.StringElementar) :
                    StringExpression, Variable

                @Serializable
                data class Concat(val left: Expression, val right: Expression) : StringExpression

                @Serializable
                data class Interpolation(val value: Expression) : StringExpression
            }

            @Serializable
            sealed interface NumberExpression : Expression {
                @Serializable
                data class NumberLiteral(override val value: Double) : NumberExpression, Literal

                @Serializable
                data class NumberVariable(override val target: DataTree.WorkingStorage.Elementar.NumberElementar) :
                    Variable, NumberExpression
            }
        }
    }
}

val String.l get() = CobolFIRTree.ProcedureTree.Expression.StringExpression.StringLiteral(this)
val Double.l get() = CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberLiteral(this)
val Int.l get() = CobolFIRTree.ProcedureTree.Expression.NumberExpression.NumberLiteral(toDouble())
