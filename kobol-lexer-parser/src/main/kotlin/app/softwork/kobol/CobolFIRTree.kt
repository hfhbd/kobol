package app.softwork.kobol

data class CobolFIRTree(
    val fileComments: List<String> = emptyList(),
    val id: ID,
    val env: EnvTree? = null,
    val data: DataTree? = null,
    val procedure: ProcedureTree
) {
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

    data class EnvTree(
        val configuration: Configuration? = null,
        val inputOutput: InputOutput? = null,
        val comments: List<String> = emptyList()
    ) {
        data class Configuration(
            val specialNames: SpecialNames? = null,
            val comments: List<String> = emptyList()
        ) {
            data class SpecialNames(
                val specialNames: List<SpecialName>,
                val comments: List<String> = emptyList()
            ) {
                data class SpecialName(val env: String, val value: String, val comments: List<String> = emptyList())
            }
        }

        data class InputOutput(
            val fileControl: FileControl? = null,
            val comments: List<String> = emptyList()
        ) {
            data class FileControl(
                val files: List<File> = emptyList(),
                val comments: List<String> = emptyList()
            ) {
                data class File(
                    val file: String,
                    val fileVariable: String,
                    val fileStatus: String,
                    val comments: List<String> = emptyList()
                )
            }
        }
    }

    data class DataTree(
        val fileSection: FileSection? = null,
        val workingStorage: List<WorkingStorage> = emptyList(),
        val comments: List<String> = emptyList()
    ) {
        data class FileSection(
            val descriptions: List<FileDescription> = emptyList(),
            val comments: List<String> = emptyList()
        ) {
            sealed interface FileDescription {
                val comments: List<String>

                data class FileDefinition(
                    val name: String,
                    val recording: String,
                    val blocks: Int? = null,
                    val records: IntRange? = null,
                    val dataRecord: String? = null,
                    override val comments: List<String> = emptyList()
                ): FileDescription
            }
        }

        sealed interface WorkingStorage {
            val comments: List<String>

            data class Record(
                val name: String,
                val elements: List<Elementar>,
                override val comments: List<String> = emptyList()
            ) : WorkingStorage, FileSection.FileDescription

            sealed interface Elementar : WorkingStorage {
                val name: String
                val formatter: Formatter?
                val value: Any?

                data class Pointer(
                    override val name: String,
                    override val formatter: Formatter? = null,
                    override val value: Any? = null,
                    override val comments: List<String> = emptyList()
                ): Elementar

                data class StringElementar(
                    override val name: String,
                    override val formatter: Formatter,
                    override val value: String? = null,
                    override val comments: List<String> = emptyList()
                ) : Elementar {
                    init {
                        if (value != null) {
                            require(!value.startsWith("\""))
                            require(!value.startsWith("'"))
                        }
                    }
                }

                sealed interface Formatter {
                    @JvmInline
                    value class Simple(val length: Int): Formatter
                    data class Custom(val parts: List<Part>): Formatter {
                        data class Part(val length: Int, val type: Type) {
                            enum class Type {
                                Space, Plus, Decimal, Zeros
                            }
                        }
                    }
                }

                data class NumberElementar(
                    override val name: String,
                    override val formatter: Formatter,
                    override val value: Number? = null,
                    override val comments: List<String> = emptyList(),
                    val signed: Boolean = false,
                    val compressed: Compressed? = null,
                    val binary: Boolean = false
                ): Elementar {
                    enum class Compressed {
                        COMP, COMP3
                    }
                }
            }
        }
    }

    data class ProcedureTree(
        val topLevel: List<Statement> = emptyList(),
        val sections: List<Section> = emptyList(),
        val comments: List<String> = emptyList()
    ) {

        data class Section(
            val name: String,
            val statements: List<Statement> = emptyList(),
            val comments: List<String> = emptyList()
        )

        sealed interface Statement {
            val comments: List<String>

            data class Move(
                val target: DataTree.WorkingStorage.Elementar,
                val value: Expression,
                override val comments: List<String> = emptyList()
            ) : Statement

            data class Display(
                val expr: Expression.StringExpression,
                override val comments: List<String> = emptyList()
            ) : Statement

            data class Perform(
                val sectionName: String,
                override val comments: List<String> = emptyList()
            ) : Statement
        }

        sealed interface Expression {
            sealed interface Literal : Expression {
                val value: Any
            }

            sealed interface Variable : Expression {
                val target: DataTree.WorkingStorage.Elementar
            }

            sealed interface StringExpression : Expression {
                data class StringLiteral(override val value: String) : StringExpression, Literal {
                    init {
                        require(!value.startsWith("\""))
                        require(!value.startsWith("'"))
                    }
                }

                data class StringVariable(override val target: DataTree.WorkingStorage.Elementar.StringElementar) :
                    StringExpression, Variable

                data class Concat(val left: StringExpression, val right: StringExpression) : StringExpression
            }
        }
    }
}
