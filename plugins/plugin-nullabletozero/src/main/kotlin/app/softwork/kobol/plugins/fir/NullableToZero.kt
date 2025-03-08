package app.softwork.kobol.plugins.fir

import app.softwork.kobol.fir.*
import app.softwork.serviceloader.ServiceLoader

@ServiceLoader(FirPluginBeforePhase::class)
public class NullableToZero : FirPluginBeforePhase {
    override fun invoke(tree: CobolFIRTree): CobolFIRTree = tree.copy(data = tree.data.toNull())
}

private fun CobolFIRTree.DataTree.toNull(): CobolFIRTree.DataTree {
    return copy(
        fileSection = fileSection.toNull(),
        workingStorage = workingStorage.toNull(),
        linkingSection = linkingSection.toNull(),
    )
}

@JvmName("ListFileToNull")
private fun List<CobolFIRTree.DataTree.File>.toNull() = map {
    it.copy(records = it.records.toNull())
}

@JvmName("ListRecordToNull")
private fun List<CobolFIRTree.DataTree.WorkingStorage.Record>.toNull() =
    map {
        it.toNull()
    }

@JvmName("ListWorkingStorageToNull")
private fun <T : CobolFIRTree.DataTree.WorkingStorage> List<T>.toNull() = map {
    it.toNull()
}

private fun <T : CobolFIRTree.DataTree.WorkingStorage> T.toNull(): T {
    return when (this) {
        is CobolFIRTree.DataTree.WorkingStorage.Elementar.EmptyElementar -> this
        is CobolFIRTree.DataTree.WorkingStorage.Elementar.NumberElementar.Normal -> copy(
            value = value ?: 0.0,
        )
        is CobolFIRTree.DataTree.WorkingStorage.Elementar.NumberElementar.ReturnCode -> this

        is CobolFIRTree.DataTree.WorkingStorage.Elementar.Pointer -> this
        is CobolFIRTree.DataTree.WorkingStorage.Elementar.StringElementar -> copy(
            value = value ?: "",
        )

        is CobolFIRTree.DataTree.WorkingStorage.Record -> copy(
            elements = elements.toNull(),
        )
    } as T
}
