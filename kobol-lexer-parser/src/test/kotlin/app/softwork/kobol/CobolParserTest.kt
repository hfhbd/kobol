package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Expression.StringExpression.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import kotlin.test.*

class CobolParserTest {
    @Test
    fun testInput() {
        val input = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 DATA                        DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 PROCEDURE                   DIVISION.
            123456* Some Comment
            123456     DISPLAY "HELLO"WORLD
            123456     MOVE "42" TO WORLD
            123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()

        assertEquals(
            CobolFIRTree(
                id = CobolFIRTree.ID(
                    programID = "HELLO"
                ),
                data = CobolFIRTree.DataTree(
                    workingStorage = listOf(
                        StringElementar(name = "WORLD", value = "WORLD!", length = 6)
                    )
                ),
                procedure = CobolFIRTree.ProcedureTree(
                    topLevel = listOf(
                        Display(
                            Concat(
                                StringLiteral("HELLO"),
                                StringVariable(target = StringElementar(name = "WORLD", value = "WORLD!", length = 6))
                            ),
                            comments = listOf("Some Comment")
                        ),
                        Move(
                            value = StringLiteral("42"),
                            target = StringElementar(name = "WORLD", value = "WORLD!", length = 6)
                        ),
                        Display(
                            Concat(
                                StringLiteral("ANSWER"),
                                StringVariable(target = StringElementar(name = "WORLD", value = "WORLD!", length = 6))
                            )
                        )
                    )
                )
            ), input.toTree()
        )
    }
}
