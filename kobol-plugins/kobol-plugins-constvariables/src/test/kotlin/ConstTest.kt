package app.softwork.kobol.plugins.ir.optimizations

import app.softwork.kobol.ir.*
import kotlin.test.*

class ConstTest {
    @Test
    fun simple() {
        val readonly = KobolIRTree.Types.Type.GlobalVariable(
            declaration = KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration(
                name = "foo",
                mutable = false,
                private = false,
                value = KobolIRTree.Expression.StringExpression.StringLiteral(""),
                comments = emptyList(),
                const = false,
                length = 1
            ),
            doc = emptyList()
        )
        val expected = KobolIRTree.Types.Type.GlobalVariable(
            declaration = KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration(
                name = "foo",
                mutable = false,
                private = false,
                value = KobolIRTree.Expression.StringExpression.StringLiteral(""),
                comments = emptyList(),
                const = true,
                length = 1
            ),
            doc = emptyList()
        )
        assertEquals(expected, readonly.constVariable())
    }

    @Test
    fun mutable() {
        val mutable = KobolIRTree.Types.Type.GlobalVariable(
            declaration = KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration(
                name = "foo",
                mutable = true,
                private = false,
                value = KobolIRTree.Expression.StringExpression.StringLiteral(""),
                comments = emptyList(),
                const = false,
                length = 1
            ),
            doc = emptyList()
        )
        assertEquals(mutable, mutable.constVariable())
    }
}
