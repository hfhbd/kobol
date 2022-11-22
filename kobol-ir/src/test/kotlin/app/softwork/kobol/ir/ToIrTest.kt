package app.softwork.kobol.ir

import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Expression.StringExpression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Type.*
import kotlin.test.*

class ToIrTest {
    @Test
    fun calling() {
        val fir = CobolFIRTree(
            fileName = "testing.cbl",
            id = ID("calling"),
            procedure = ProcedureTree(
                sections = build {
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
                }
            )
        )

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
            id = "testing.cbl",
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
