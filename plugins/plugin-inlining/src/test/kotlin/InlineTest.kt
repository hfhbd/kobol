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
            length = 1
        )
        val globalVariable = GlobalVariable(string, doc = emptyList())

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +Print(StringVariable(string))
            },
            types = build {
                +globalVariable
            }
        )

        val after = Inlining().invoke(before, emptyList()).single()

        assertEquals(
            KobolIRTree(
                name = "inlining",
                id = "inlining",
                main = KobolIRTree.Types.Function(name = "main") {
                    +string.copy(mutable = false)
                    +Print(StringVariable(string))
                },
                types = emptyList()
            ), after
        )
    }

    @Test
    fun simpleTwice() {
        val string = StringDeclaration(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1
        )
        val globalVariable = GlobalVariable(string, doc = emptyList())

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +Print(StringVariable(string))
            },
            types = build {
                +globalVariable
                +function("foo") {
                    +Print(StringVariable(string))
                }
            }
        )

        val after = Inlining().invoke(before, emptyList()).single()

        assertEquals(before, after)
    }

    @Test
    fun parameters() {
        val string = StringDeclaration(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1
        )
        val globalVariable = GlobalVariable(string, doc = emptyList())

        val baz by function(parameters = listOf(string.copy(mutable = false))) {}

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +baz(globalVariable)
            },
            types = build {
                +globalVariable
                +baz
            }
        )

        val after = Inlining().invoke(before, emptyList()).single()

        assertEquals(
            KobolIRTree(
                name = "inlining",
                id = "inlining",
                main = KobolIRTree.Types.Function(name = "main") {
                    +string.copy(mutable = false)
                    +baz(StringVariable(string.copy(mutable = false)))
                },
                types = build {
                    +baz
                }
            ), after
        )
    }

    @Test
    fun forEach() {
        val counter = IntDeclaration(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1,
            const = false
        )
        val globalVariable = GlobalVariable(counter, doc = emptyList())

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +For(counter = counter, from = 1.l, condition = Smaller(IntVariable(counter), 2.l)) {

                }
            },
            types = build {
                +globalVariable
            }
        )

        val after = Inlining().invoke(before, emptyList()).single()

        assertEquals(KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +counter
                +For(counter = counter, from = 1.l, condition = Smaller(IntVariable(counter), 2.l)) {

                }
            },
            types = emptyList()
        ), after)
    }
}
