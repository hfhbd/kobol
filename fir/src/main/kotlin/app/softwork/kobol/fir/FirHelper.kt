package app.softwork.kobol.fir

import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.NumberExpression.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.StringExpression.*

public val String.l: StringLiteral get() = StringLiteral(this)
public val Double.l: NumberLiteral get() = NumberLiteral(this)
public val Int.l: NumberLiteral get() = NumberLiteral(toDouble())
