package app.softwork.kobol

data class CobolFIRTree(
    val fileComments: List<String>,
    val id: ID,
    val env: EnvTree?,
    val data: DataTree?,
    val procedure: ProcedureTree
) {
    data class ID(
        val programID: String,
        val programIDComments: List<String>,
        val author: String,
        val authorComments: List<String>,
        val installation: String,
        val installationsComments: List<String>,
        val date: String,
        val dateComments: List<String>
    )

    object EnvTree
    data class DataTree(
        val workingStorage: List<WorkingStorage>,
        val comments: List<String>
    ) {
        sealed interface WorkingStorage {
            sealed interface Elementar : WorkingStorage {
                val name: String

                data class StringElementar(
                    override val name: String,
                    val length: Int,
                    val value: String?,
                    val comments: List<String>
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
        val topLevel: List<Statement>,
        val sections: List<Section>,
        val comments: List<String>
    ) {

        data class Section(
            val name: String,
            val statements: List<Statement>,
            val comments: List<String>
        )

        sealed interface Statement {
            val comments: List<String>

            data class Move(
                val target: DataTree.WorkingStorage.Elementar,
                val value: Expression,
                override val comments: List<String>
            ) : Statement

            data class Display(
                val expr: Expression.StringExpression,
                override val comments: List<String>
            ) : Statement

            data class Perform(
                val sectionName: String,
                override val comments: List<String>
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
