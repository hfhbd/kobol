package app.softwork.kobol.plugins.fir

import app.softwork.kobol.*
import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Statement.*
import kotlin.test.*

class RemoveExitProcessTest {
    @Test
    fun noExit() {
        val before by cobolFir {
            procedure {
                topLevel {
                    +If("".l eq 2.l, statements = build {
                        +GoBack()
                    })
                }
                section("foo") {
                    +GoBack()
                }
            }
        }

        val after = cobolFir("before") {
            procedure {
                topLevel {
                    +If("".l eq 2.l, statements = emptyList())
                }
                section("foo") {

                }
            }
        }
        assertEquals(after, RemoveExitProcess().invoke(before))
    }
}
