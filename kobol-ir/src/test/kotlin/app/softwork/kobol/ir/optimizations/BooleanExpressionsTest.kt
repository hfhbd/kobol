package app.softwork.kobol.ir.optimizations

import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Expression.BooleanExpression.*
import kotlin.test.*

class BooleanExpressionsTest {
    @Test
    fun optimize() {
        assertEquals(false.l, Not(true.l).optimize())
        assertEquals(true.l, Not(false.l).optimize())

        assertEquals(false.l, (!(true.l eq true.l)).optimize())

        assertEquals(true.l, (true.l and true.l).optimize())
        assertEquals(true.l, (true.l or true.l).optimize())

        assertEquals(true.l, (true.l or (true.l and !true.l)).optimize())
        assertEquals(false.l, (false.l or (true.l and !true.l)).optimize())

        assertEquals(false.l, (false.l or (true.l and !true.l)).optimize())

        assertEquals(false.l, (!(false.l or true.l)).optimize())
    }

    private val Boolean.l get() = BooleanLiteral(this)
    private infix fun KobolIRTree.Expression.BooleanExpression.and(right: KobolIRTree.Expression.BooleanExpression) =
        And(this, right)

    private infix fun KobolIRTree.Expression.BooleanExpression.or(right: KobolIRTree.Expression.BooleanExpression) =
        Or(this, right)

    private operator fun KobolIRTree.Expression.BooleanExpression.not() =
        Not(this)

    private infix fun KobolIRTree.Expression.BooleanExpression.eq(right: KobolIRTree.Expression.BooleanExpression) =
        Eq(this, right)
}
