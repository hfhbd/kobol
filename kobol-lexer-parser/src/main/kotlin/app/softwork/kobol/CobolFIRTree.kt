package app.softwork.kobol

data class CobolFIRTree(
    val id: ID,
    val env: EnvTree?,
    val data: DataTree?,
    val procedure: ProcedureTree
) {
    data class ID(
        val programID: String,
        val author: String,
        val installation: String,
        val date: String
    )

    object EnvTree
    data class DataTree(
        val workingStorage: List<WorkingStorage>
    ) {
        sealed interface WorkingStorage {
            sealed interface Elementar : WorkingStorage {
                val name: String

                data class StringElementar(override val name: String, val length: Int, val value: String?) :
                    Elementar {
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

    data class ProcedureTree(val content: List<Procs>) {
        sealed interface Procs {
            data class TopLevelStatements(val statements: List<Statement>) : Procs

            data class Section(val name: String, val statements: List<Statement>) : Procs
        }

        sealed interface Statement {
            data class Move(val target: DataTree.WorkingStorage.Elementar, val value: Expression) : Statement
            data class Display(val expr: Expression.StringExpression) : Statement
            data class Perform(val sectionName: String) : Statement
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
