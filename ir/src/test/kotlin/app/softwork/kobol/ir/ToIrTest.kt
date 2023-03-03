package app.softwork.kobol.ir

import app.softwork.kobol.*
import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.fir.l
import app.softwork.kobol.ir.KobolIRTree.Expression.StringExpression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import app.softwork.kobol.ir.KobolIRTree.Types.Type.*
import app.softwork.kobol.plugins.ir.ExitProcessControlFlowHandlingFactory.*
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
                        +StopRun()
                    }
                    +Section("BAR") {
                        +Display("BAR".l)
                        +StopRun()
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
            +exit()
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
            +exit()
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
                +RC
            }
        )
        assertEquals(expected = ir, actual = fir.toIRTree(
            controlFlowHandling = {
                ExitProcessControlFlowHandling
            }
        ))
    }
}
