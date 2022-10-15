package app.softwork.kobol.optimizations

import app.softwork.kobol.*
import kotlin.test.*

class ConstTest {
    @Test
    fun simple() {
        val readonly = KobolIRTree.Types.Type.GlobalVariable(
            declaration = KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration(
                name = "foo",
                className = null,
                mutable = false,
                private = false,
                value = KobolIRTree.Expression.StringExpression.StringLiteral(""),
                comments = emptyList(),
                const = false,
            ),
            doc = emptyList()
        )
        val expected = KobolIRTree.Types.Type.GlobalVariable(
            declaration = KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration(
                name = "foo",
                className = null,
                mutable = false,
                private = false,
                value = KobolIRTree.Expression.StringExpression.StringLiteral(""),
                comments = emptyList(),
                const = true,
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
                className = null,
                mutable = true,
                private = false,
                value = KobolIRTree.Expression.StringExpression.StringLiteral(""),
                comments = emptyList(),
                const = false,
            ),
            doc = emptyList()
        )
        assertEquals(mutable, mutable.constVariable())
    }
}
