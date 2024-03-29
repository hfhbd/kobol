package app.softwork.kobol.ir

import app.softwork.kobol.*
import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.DataTree.WorkingStorage.Elementar.NumberElementar.ReturnCode
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import app.softwork.kobol.fir.l
import app.softwork.kobol.ir.KobolIRTree.*
import app.softwork.kobol.ir.KobolIRTree.Expression.BooleanExpression.*
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.*
import app.softwork.kobol.plugins.ir.*
import app.softwork.kobol.plugins.ir.optimizations.*
import kotlin.test.*

class ExitTest {
    @Test
    fun replaceExitTest() {
        val before by cobolFir {
            procedure {
                topLevel {
                    +Move(ReturnCode(), 8.l)
                    +If(
                        1.l eq 1.l,
                        buildList {
                            +StopRun()
                        },
                    )
                }
            }
        }

        val after = before.toIRTree(
            controlFlowHandling = {
                ExitProcessControlFlowHandlingFactory
            },
        )

        assertEquals(
            KobolIRTree(
                name = "before",
                id = "",
                main = Types.Function(name = "before") {
                    +Statement.Assignment(testingReturnCodeIr, IntLiteral(8))
                    +Statement.If(
                        Eq(IntLiteral(1), IntLiteral(1)),
                        statements = buildList {
                            +Statement.Exit(testingReturnCodeIr.variable() as IntVariable)
                        },
                    )
                }.copy(isEntryPoint = true),
                types = buildList {
                    +RC
                },
            ),
            after,
        )
    }

    @Test
    fun replaceExitTestWithInlining() {
        val before by cobolFir {
            procedure {
                topLevel {
                    +If(
                        1.l eq 1.l,
                        buildList {
                            +StopRun()
                        },
                    )
                }
            }
        }

        val after = NoSynthetics().invoke(
            tree = before.toIRTree(
                controlFlowHandling = {
                    ExitProcessControlFlowHandlingFactory
                },
            ),
            emptyList(),
        ).single()

        assertEquals(
            KobolIRTree(
                name = "before",
                id = "",
                main = Types.Function(name = "before") {
                    +Statement.If(
                        Eq(IntLiteral(1), IntLiteral(1)),
                        statements = buildList {
                            +Statement.Exit(IntLiteral(0))
                        },
                    )
                }.copy(isEntryPoint = true),
                types = emptyList(),
            ),
            after,
        )
    }

    @Test
    fun replaceExitTestWithVariableInlining() {
        val before by cobolFir {
            procedure {
                topLevel {
                    +Move(ReturnCode(), 8.l)
                    +If(
                        1.l eq 1.l,
                        buildList {
                            +StopRun()
                        },
                    )
                }
            }
        }

        val after = Inlining().invoke(
            tree = before.toIRTree(
                controlFlowHandling = {
                    ExitProcessControlFlowHandlingFactory
                },
            ),
            emptyList(),
        ).single()

        assertEquals(
            KobolIRTree(
                name = "before",
                id = "",
                main = Types.Function(name = "before") {
                    +testingReturnCodeIr
                    +Statement.Assignment(testingReturnCodeIr, IntLiteral(8))
                    +Statement.If(
                        Eq(IntLiteral(1), IntLiteral(1)),
                        statements = buildList {
                            +Statement.Exit(testingReturnCodeIr.variable() as IntVariable)
                        },
                    )
                }.copy(isEntryPoint = true),
                types = emptyList(),
            ),
            after,
        )
    }
}
