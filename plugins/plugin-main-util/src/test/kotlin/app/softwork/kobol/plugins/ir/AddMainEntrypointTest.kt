package app.softwork.kobol.plugins.ir

import app.softwork.kobol.ir.KobolIRTree
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Declaration
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.Print
import app.softwork.kobol.ir.KobolIRTree.Types.Type.Natives
import app.softwork.kobol.ir.function
import app.softwork.kobol.ir.invoke
import app.softwork.kobol.ir.l
import app.softwork.kobol.unaryPlus
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class AddMainEntrypointTest {
    @Test
    fun mainAdded() {
        val normalMain = function("test") {
            +Print("Hello World".l)
        }

        val before = KobolIRTree(
            "test",
            "test",
            main = normalMain,
            types = emptyList(),
        )

        val expected = KobolIRTree(
            "test",
            "test",
            main = normalMain,
            types = buildList {
                +function("main") {
                    +normalMain()
                }.copy(parameters = stringArgs)
            },
        )

        assertEquals(
            expected,
            before.addMainEntrypoint { main, _ ->
                +main()
            },
        )
    }

    @Test
    fun mainAlreadyAddedThrows() {
        val normalMain = function("test") {
            +Print("Hello World".l)
        }

        val expected = KobolIRTree(
            "test",
            "test",
            main = normalMain,
            types = buildList {
                +function("main") {
                    +normalMain()
                }.copy(parameters = stringArgs)
            },
        )

        assertFailsWith<IllegalStateException> {
            expected.addMainEntrypoint { main, _ ->
                +main()
            }
        }
    }

    @Test
    fun similarMainDontThrows() {
        val normalMain = function("test") {
            +Print("Hello World".l)
        }

        val similarMain = function("main") {
        }.copy(
            parameters = listOf(
                Declaration.Array(
                    name = "args",
                    type = Natives.Int,
                    nullable = false,
                    mutable = false,
                    private = false,
                ),
            ),
        )

        val before = KobolIRTree(
            "test",
            "test",
            main = normalMain,
            types = listOf(similarMain),
        )

        val expected = KobolIRTree(
            "test",
            "test",
            main = normalMain,
            types = buildList {
                +similarMain
                +function("main") {
                    +normalMain()
                }.copy(parameters = stringArgs)
            },
        )

        assertEquals(
            expected,
            before.addMainEntrypoint { main, _ ->
                +main()
            },
        )
    }
}
