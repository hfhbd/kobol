package app.softwork.kobol

import app.softwork.kobol.fir.CobolFIRTree

public fun interface ProcedureName {
    public fun name(fileName: String, id: CobolFIRTree.ID): String
}
