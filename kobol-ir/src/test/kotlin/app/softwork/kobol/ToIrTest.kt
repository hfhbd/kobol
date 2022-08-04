package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.KobolIRTree.Expression.StringExpression.*
import app.softwork.kobol.KobolIRTree.Types.Function
import app.softwork.kobol.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.KobolIRTree.Types.Type.*
import kotlin.test.*

class ToIrTest {
    @Test
    fun calling() {
        val fir = CobolFIRTree(id = ID("calling"), procedure = ProcedureTree(sections = build {
            +Section(name = "FOO") {
                +Display("FOO".l)
                +Perform("BAR")
                +Display("FOO2".l)
                +GoBack()
            }
            +Section("BAR") {
                +Display("BAR".l)
                +GoBack()
            }
            +Section("C") {
                +Display("C".l)
            }
        }))

        val c = Function("C") {
            +Print(StringLiteral("C"), emptyList())
        }
        val bar = Function("BAR") {
            +Print(StringLiteral("BAR"), emptyList())
            +Exit(emptyList())
        }

        val foo = Function(
            name = "FOO",
            parameters = emptyList(),
            returnType = Void,
            private = false,
        ) {
            +Print(StringLiteral("FOO"), emptyList())
            +FunctionCall(bar.declaration(), emptyList(), emptyList())
            +Print(StringLiteral("FOO2"), emptyList())
            +Exit(emptyList())
        }

        val ir = KobolIRTree(
            name = "calling",
            main = Function(
                name = "main",
                parameters = emptyList(),
                returnType = Void,
                body = build {
                    +FunctionCall(foo.declaration(), emptyList(), emptyList())
                    +FunctionCall(bar.declaration(), emptyList(), emptyList())
                    +FunctionCall(c.declaration(), emptyList(), emptyList())
                },
                private = false,
                doc = emptyList()
            ),
            types = build {
                +foo
                +bar
                +c
            }
        )
        assertEquals(expected = ir, actual = fir.toIRTree())
    }
}
