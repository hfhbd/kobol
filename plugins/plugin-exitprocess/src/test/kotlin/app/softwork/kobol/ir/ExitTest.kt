package app.softwork.kobol.ir

import app.softwork.kobol.fir.CobolFIRTree
import app.softwork.kobol.fir.cobolFir
import app.softwork.kobol.fir.eq
import app.softwork.kobol.fir.getValue
import app.softwork.kobol.fir.l
import app.softwork.kobol.ir.KobolIRTree.Expression.NumberExpression.IntExpression.IntVariable
import app.softwork.kobol.plugins.ir.ExitProcessControlFlowHandlingFactory
import app.softwork.kobol.plugins.ir.optimizations.Inlining
import app.softwork.kobol.plugins.ir.optimizations.NoSynthetics
import app.softwork.kobol.toIRTree
import app.softwork.kobol.unaryPlus
import kotlin.test.Test
import kotlin.test.assertEquals

class ExitTest {
    @Test
    fun replaceExitTest() {
        val before by cobolFir {
            procedure {
                topLevel {
                    +CobolFIRTree.ProcedureTree.Statement.Move(
                        CobolFIRTree.DataTree.WorkingStorage.Elementar.NumberElementar.ReturnCode(),
                        8.l,
                    )
                    +CobolFIRTree.ProcedureTree.Statement.If(
                        1.l eq 1.l,
                        buildList {
                            +CobolFIRTree.ProcedureTree.Statement.StopRun()
                        },
                    )
                }
            }
        }

        val after = before.toIRTree(
            controlFlowHandling = {
                ExitProcessControlFlowHandlingFactory.ExitProcessControlFlowHandling
            },
        )

        assertEquals(
            KobolIRTree(
                name = "before",
                id = "",
                main = KobolIRTree.Types.Function(name = "before") {
                    +KobolIRTree.Types.Function.Statement.Assignment(
                        testingReturnCodeIr,
                        KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(8),
                    )
                    +KobolIRTree.Types.Function.Statement.If(
                        KobolIRTree.Expression.BooleanExpression.Eq(
                            KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(
                                1,
                            ),
                            KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(1),
                        ),
                        statements = buildList {
                            +KobolIRTree.Types.Function.Statement.Exit(
                                testingReturnCodeIr.variable() as IntVariable,
                            )
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
                    +CobolFIRTree.ProcedureTree.Statement.If(
                        1.l eq 1.l,
                        buildList {
                            +CobolFIRTree.ProcedureTree.Statement.StopRun()
                        },
                    )
                }
            }
        }

        val after = NoSynthetics().invoke(
            tree = before.toIRTree(
                controlFlowHandling = {
                    ExitProcessControlFlowHandlingFactory.ExitProcessControlFlowHandling
                },
            ),
            emptyList(),
        ).single()

        assertEquals(
            KobolIRTree(
                name = "before",
                id = "",
                main = KobolIRTree.Types.Function(name = "before") {
                    +KobolIRTree.Types.Function.Statement.If(
                        KobolIRTree.Expression.BooleanExpression.Eq(
                            KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(
                                1,
                            ),
                            KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(1),
                        ),
                        statements = buildList {
                            +KobolIRTree.Types.Function.Statement.Exit(
                                KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(
                                    0,
                                ),
                            )
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
                    +CobolFIRTree.ProcedureTree.Statement.Move(
                        CobolFIRTree.DataTree.WorkingStorage.Elementar.NumberElementar.ReturnCode(),
                        8.l,
                    )
                    +CobolFIRTree.ProcedureTree.Statement.If(
                        1.l eq 1.l,
                        buildList {
                            +CobolFIRTree.ProcedureTree.Statement.StopRun()
                        },
                    )
                }
            }
        }

        val after = Inlining().invoke(
            tree = before.toIRTree(
                controlFlowHandling = {
                    ExitProcessControlFlowHandlingFactory.ExitProcessControlFlowHandling
                },
            ),
            emptyList(),
        ).single()

        assertEquals(
            KobolIRTree(
                name = "before",
                id = "",
                main = KobolIRTree.Types.Function(name = "before") {
                    +testingReturnCodeIr
                    +KobolIRTree.Types.Function.Statement.Assignment(
                        testingReturnCodeIr,
                        KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(8),
                    )
                    +KobolIRTree.Types.Function.Statement.If(
                        KobolIRTree.Expression.BooleanExpression.Eq(
                            KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(
                                1,
                            ),
                            KobolIRTree.Expression.NumberExpression.IntExpression.IntLiteral(1),
                        ),
                        statements = buildList {
                            +KobolIRTree.Types.Function.Statement.Exit(
                                testingReturnCodeIr.variable() as IntVariable,
                            )
                        },
                    )
                }.copy(isEntryPoint = true),
                types = emptyList(),
            ),
            after,
        )
    }
}
