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

    object EnvTree
    data class DataTree(
        val workingStorage: List<WorkingStorage>,
        val comments: List<String> = emptyList()
    ) {
        sealed interface WorkingStorage {
            sealed interface Elementar : WorkingStorage {
                val name: String

                data class StringElementar(
                    override val name: String,
                    val length: Int,
                    val value: String?,
                    val comments: List<String> = emptyList()
                ) : Elementar {
                    init {
                        if (value != null) {
                            require(!value.startsWith("\""))
                            require(!value.startsWith("'"))
                        }
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
