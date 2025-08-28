package app.softwork.kobol.optimizations

import app.softwork.kobol.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.plugins.ir.optimizations.*
import kotlin.test.*

class InlineTest {
    @Test
    fun simpleOnce() {
        val string = KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1,
        )
        val globalVariable = KobolIRTree.Types.Type.GlobalVariable(string, doc = emptyList())

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +KobolIRTree.Types.Function.Statement.Print(
                    KobolIRTree.Expression.StringExpression.StringVariable(
                        string,
                    ),
                )
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
                    +KobolIRTree.Types.Function.Statement.Print(
                        KobolIRTree.Expression.StringExpression.StringVariable(
                            string.copy(mutable = false),
                        ),
                    )
                },
                types = emptyList(),
            ),
            after,
        )
    }

    @Test
    fun simpleTwice() {
        val string = KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1,
        )
        val globalVariable = KobolIRTree.Types.Type.GlobalVariable(string, doc = emptyList())

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +KobolIRTree.Types.Function.Statement.Print(
                    KobolIRTree.Expression.StringExpression.StringVariable(
                        string,
                    ),
                )
            },
            types = buildList {
                +globalVariable
                +function("foo") {
                    +KobolIRTree.Types.Function.Statement.Print(
                        KobolIRTree.Expression.StringExpression.StringVariable(
                            string,
                        ),
                    )
                }
            },
        )

        val after = before.inlineGlobalVariables(false)

        assertEquals(before, after)
    }

    @Test
    fun parameters() {
        val string = KobolIRTree.Types.Function.Statement.Declaration.StringDeclaration(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1,
        )
        val globalVariable = KobolIRTree.Types.Type.GlobalVariable(string, doc = emptyList())

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
                    +baz(KobolIRTree.Expression.StringExpression.StringVariable(string.copy(mutable = false)))
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
        val counter = KobolIRTree.Types.Function.Statement.Declaration.IntDeclaration.Normal(
            name = "A",
            value = null,
            mutable = true,
            private = false,
            length = 1,
            const = false,
            isSigned = false,
        )
        val globalVariable = KobolIRTree.Types.Type.GlobalVariable(counter, doc = emptyList())

        val before = KobolIRTree(
            name = "inlining",
            id = "inlining",
            main = KobolIRTree.Types.Function(name = "main") {
                +KobolIRTree.Types.Function.Statement.For(
                    counter = counter,
                    from = 1.l,
                    condition = KobolIRTree.Expression.BooleanExpression.Smaller(
                        KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable(counter),
                        2.l,
                    ),
                ) {
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
                    +KobolIRTree.Types.Function.Statement.For(
                        counter = counter,
                        from = 1.l,
                        condition = KobolIRTree.Expression.BooleanExpression.Smaller(
                            KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable(counter),
                            2.l,
                        ),
                    ) {
                    }
                },
                types = emptyList(),
            ),
            after,
        )
    }
}
