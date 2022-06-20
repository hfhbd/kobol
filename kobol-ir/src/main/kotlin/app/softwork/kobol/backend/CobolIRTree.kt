package app.softwork.kobol.backend

data class CobolIRTree(
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
        sealed class WorkingStorage {
            data class Elementar
        }
    }
    data class ProcedureTree()
}
