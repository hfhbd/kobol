package app.softwork.kobol.ir

import app.softwork.kobol.fir.CobolFIRTree

public fun interface ProcedureName {
    public fun name(fileName: String, id: CobolFIRTree.ID): String
}
