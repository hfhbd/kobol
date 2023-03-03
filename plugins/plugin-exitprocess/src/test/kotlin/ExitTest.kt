package app.softwork.kobol.ir

import app.softwork.kobol.*
import app.softwork.kobol.fir.*
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
                    +Move(returnCodeElementar, 8.l)
                    +If(1.l eq 1.l, build {
                        +StopRun()
                    })
                }
            }
        }

        val after = before.toIRTree(
            controlFlowHandling = {
                ExitProcessControlFlowHandlingFactory
            }
        )

        assertEquals(
            KobolIRTree(
                name = "before",
                id = "",
                main = Types.Function(name = "main") {
                    +Statement.Assignment(testingReturnCodeIr, IntLiteral(8))
                    +Statement.If(
                        Eq(IntLiteral(1), IntLiteral(1)),
                        statements = build {
                            +Statement.Exit(testingReturnCodeIr.variable() as IntVariable)
                        }
                    )
                },
                types = build { 
                    +RC
                }
            ), after
        )
    }

    @Test
    fun replaceExitTestWithInlining() {
        val before by cobolFir {
            procedure {
                topLevel {
                    +If(1.l eq 1.l, build {
                        +StopRun()
                    })
                }
            }
        }

        val after = NoSynthetics().invoke(
            tree = before.toIRTree(
                controlFlowHandling = {
                    ExitProcessControlFlowHandlingFactory
                },
            ), emptyList()
        ).single()

        assertEquals(
            KobolIRTree(
                name = "before",
                id = "",
                main = Types.Function(name = "main") {
                    +Statement.If(
                        Eq(IntLiteral(1), IntLiteral(1)),
                        statements = build {
                            +Statement.Exit(IntLiteral(0))
                        }
                    )
                },
                types = emptyList()
            ), after
        )
    }
    
    @Test
    fun replaceExitTestWithVariableInlining() {
        val before by cobolFir {
            procedure {
                topLevel {
                    +Move(returnCodeElementar, 8.l)
                    +If(1.l eq 1.l, build {
                        +StopRun()
                    })
                }
            }
        }

        val after = Inlining().invoke(
            tree = before.toIRTree(
                controlFlowHandling = {
                    ExitProcessControlFlowHandlingFactory
                },
            ), emptyList()
        ).single()

        assertEquals(
            KobolIRTree(
                name = "before",
                id = "",
                main = Types.Function(name = "main") {
                    +testingReturnCodeIr
                    +Statement.Assignment(testingReturnCodeIr, IntLiteral(8))
                    +Statement.If(
                        Eq(IntLiteral(1), IntLiteral(1)),
                        statements = build {
                            +Statement.Exit(testingReturnCodeIr.variable() as IntVariable)
                        }
                    )
                },
                types = emptyList()
            ), after
        )
    }
}
