package app.softwork.kobol.fir

import app.softwork.kobol.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.BooleanExpression.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.NumberExpression.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.StringExpression.*
import kotlin.reflect.*

public val String.l: StringLiteral get() = StringLiteral(this)
public val Double.l: NumberLiteral get() = NumberLiteral(this)
public val Int.l: NumberLiteral get() = NumberLiteral(toDouble())

public infix fun Expression.eq(right: Expression): Equals = Equals(this, right)

public operator fun CobolFIRTree.getValue(receiver: Any?, prop: KProperty<*>): CobolFIRTree = copy(
    id = id.copy(programID = prop.name)
)

@DslMarker
public annotation class CobolFIRTreeMarker

public fun cobolFir(programID: String = "", build: CobolFIRTreeBuilder.() -> Unit): CobolFIRTree = CobolFIRTreeBuilder(programID).apply(build).build()

@CobolFIRTreeMarker
public class CobolFIRTreeBuilder internal constructor(private val programID: String){
    private lateinit var procedure: CobolFIRTree.ProcedureTree

    @CobolFIRTreeMarker
    public class ProcedureBuilder {
        private var topLevel: List<Statement> = emptyList()
        private val sections: MutableList<Section> = mutableListOf()

        @CobolFIRTreeMarker
        public fun topLevel(builder: Builder<Statement>.() -> Unit) {
            topLevel = build(builder)
        }

        @CobolFIRTreeMarker
        public fun section(name: String, builder: Builder<Statement>.() -> Unit) {
            sections.add(Section(name, statements = build(builder)))
        }

        internal fun build(): CobolFIRTree.ProcedureTree = CobolFIRTree.ProcedureTree(topLevel)
    }

    @CobolFIRTreeMarker
    public fun procedure(action: ProcedureBuilder.() -> Unit) {
        procedure = ProcedureBuilder().apply(action).build()
    }

    internal fun build(): CobolFIRTree = CobolFIRTree(
        fileName = "",
        id = CobolFIRTree.ID(programID),
        procedure = procedure
    )
} 
