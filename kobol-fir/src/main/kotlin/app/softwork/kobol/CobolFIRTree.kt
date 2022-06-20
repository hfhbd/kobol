package app.softwork.kobol

/**
 * Untyped Frontend IR-Tree
 */
data class CobolFIRTree(
    val id: ID,
    val env: EnvTree,
    val data: DataTree,
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
        val workingStorage: WorkingStorage
    ) {
        sealed interface WorkingStorage {
            data class Elementar(val name: String, val type: String, val length: Int, val value: String?):
                WorkingStorage
        }
    }
    data class ProcedureTree(val functions: List<Function>) {
        data class Function(val name: String, val body: List<Statement>) {
            sealed interface Statement {
                data class Move(val target: DataTree.WorkingStorage.Elementar, val value: Expression): Statement
                data class Display(val expr: Expression): Statement
            }
        }
        sealed interface Expression {
            data class Literal(val value: String): Expression
            data class Concat(val left: Expression, val right: Expression): Expression
        }
    }
}
