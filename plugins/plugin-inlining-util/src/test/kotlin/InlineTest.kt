package app.softwork.kobol.optimizations

import app.softwork.kobol.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Expression.BooleanExpression.*
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression.*
import app.softwork.kobol.ir.KobolIRTree.Expression.StringExpression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration.*
import app.softwork.kobol.ir.KobolIRTree.Types.Type.*
import app.softwork.kobol.plugins.ir.optimizations.*
import kotlin.test.*

class InlineTest {
    @Test
    fun simpleOnce() {
        val string = StringDeclaration(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1,
        )
        val globalVariable = GlobalVariable(string, doc = emptyList())

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +Print(StringVariable(string))
            },
            types = buildList {
                +globalVariable
            },
        )

        val after = before.inlineGlobalVariables(false)

        assertEquals(
            KobolIRTree(
                name = "inlining",
                id = "inlining",
                main = KobolIRTree.Types.Function(name = "main") {
                    +string.copy(mutable = false)
                    +Print(StringVariable(string.copy(mutable = false)))
                },
                types = emptyList(),
            ),
            after,
        )
    }

    @Test
    fun simpleTwice() {
        val string = StringDeclaration(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1,
        )
        val globalVariable = GlobalVariable(string, doc = emptyList())

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +Print(StringVariable(string))
            },
            types = buildList {
                +globalVariable
                +function("foo") {
                    +Print(StringVariable(string))
                }
            },
        )

        val after = before.inlineGlobalVariables(false)

        assertEquals(before, after)
    }

    @Test
    fun parameters() {
        val string = StringDeclaration(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1,
        )
        val globalVariable = GlobalVariable(string, doc = emptyList())

        val baz by function(parameters = listOf(string.copy(mutable = false))) {}

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +baz(globalVariable)
            },
            types = buildList {
                +globalVariable
                +baz
            },
        )

        val after = before.inlineGlobalVariables(false)

        assertEquals(
            KobolIRTree(
                name = "inlining",
                id = "inlining",
                main = KobolIRTree.Types.Function(name = "main") {
                    +string.copy(mutable = false)
                    +baz(StringVariable(string.copy(mutable = false)))
                },
                types = buildList {
                    +baz
                },
            ),
            after,
        )
    }

    @Test
    fun forEach() {
        val counter = IntDeclaration.Normal(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1,
            const = false,
            isSigned = false,
        )
        val globalVariable = GlobalVariable(counter, doc = emptyList())

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +For(counter = counter, from = 1.l, condition = Smaller(IntVariable(counter), 2.l)) {
                }
            },
            types = buildList {
                +globalVariable
            },
        )

        val after = before.inlineGlobalVariables(false)

        assertEquals(
            KobolIRTree(
                name = "inlining",
                id = "inlining",
                main = KobolIRTree.Types.Function(name = "main") {
                    +counter
                    +For(counter = counter, from = 1.l, condition = Smaller(IntVariable(counter), 2.l)) {
                    }
                },
                types = emptyList(),
            ),
            after,
        )
    }
}
