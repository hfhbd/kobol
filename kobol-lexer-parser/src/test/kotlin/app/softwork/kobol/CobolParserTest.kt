package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.Formatter.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.Formatter.Custom.Part.*
import app.softwork.kobol.CobolFIRTree.EnvTree.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Expression.StringExpression.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import kotlin.test.*

class CobolParserTest {
    @Test
    fun data() {
        val input = """
                123456 IDENTIFICATION DIVISION.
                123456 PROGRAM-ID. HELLO.
                123456 DATA DIVISION.
                123456 FILE SECTION.
                123456 FD FOO
                123456     RECORDING               V
                123456     LABEL RECORD            STANDARD
                123456     DATA RECORD             BAR.
                123456 01 BAR.
                123456    05 FOO PIC X(3).
                123456    05 BAR PIC A(3).            
                123456 WORKING-STORAGE SECTION.
                123456 01 RPI.
                123456 05 WORLD PIC X(6) VALUE 'WORLD!'.
                123456 05 ANSWER PIC A(6).
                123456 77 FOO PIC X(6) VALUE '123456'.
                123456 01 RPICA.
                123456    05 FOO PIC A(3).
                123456 77 BAR PIC A(3).
                123456 PROCEDURE DIVISION.
                123456 DISPLAY WORLD
                123456 DISPLAY FOO
                123456 DISPLAY FOO OF RPICA.
            """.trimIndent()
        assertEquals(
            CobolFIRTree(
                id = CobolFIRTree.ID(
                    programID = "HELLO"
                ),
                data = CobolFIRTree.DataTree(
                    workingStorage = build {
                        +Record(name = "RPI") {
                            +StringElementar(name = "WORLD", value = "WORLD!", formatter = Simple(6))
                            +StringElementar(name = "ANSWER", formatter = Simple(6))
                        }
                        +StringElementar(name = "FOO", value = "123456", formatter = Simple(6))
                        +Record(name = "RPICA") {
                            +StringElementar(name = "FOO", formatter = Simple(3))
                        }
                        +StringElementar(name = "BAR", formatter = Simple(3))
                    }
                ),
                procedure = CobolFIRTree.ProcedureTree(
                    topLevel = build {
                        +Display(
                            StringVariable(
                                target = StringElementar(name = "WORLD", value = "WORLD!", formatter = Simple(6))
                            )
                        )
                        +Display(
                            StringVariable(
                                target = StringElementar(name = "FOO", value = "123456", formatter = Simple(6))
                            )
                        )
                        +Display(
                            StringVariable(
                                target = StringElementar(name = "FOO", formatter = Simple(3))
                            )
                        )
                    }
                )
            ), input.toTree()
        )
    }

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
                ), data = CobolFIRTree.DataTree(
                    workingStorage = listOf(
                        StringElementar(name = "WORLD", value = "WORLD!", formatter = Simple(6))
                    )
                ), procedure = CobolFIRTree.ProcedureTree(
                    topLevel = listOf(
                        Display(
                            Concat(
                                StringLiteral("HELLO"), StringVariable(
                                    target = StringElementar(
                                        name = "WORLD", value = "WORLD!", formatter = Simple(6)
                                    )
                                )
                            ), comments = listOf("Some Comment")
                        ), Move(
                            value = StringLiteral("42"),
                            target = StringElementar(
                                name = "WORLD",
                                value = "WORLD!",
                                formatter = Simple(6)
                            )
                        ), Display(
                            Concat(
                                StringLiteral("ANSWER"), StringVariable(
                                    target = StringElementar(
                                        name = "WORLD", value = "WORLD!", formatter = Simple(6)
                                    )
                                )
                            )
                        )
                    )
                )
            ), input.toTree()
        )
    }

    @Test
    fun specialNames() {
        val input = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 ENVIRONMENT DIVISION.
            123456 CONFIGURATION SECTION.
            123456 SPECIAL-NAMES.
            123456     DECIMAL-POINT           IS COMMA.
            123456 PROCEDURE                   DIVISION.
            123456     DISPLAY "HELLO".
        """.trimIndent()

        assertEquals(
            CobolFIRTree(
                id = CobolFIRTree.ID(
                    programID = "HELLO"
                ), env = CobolFIRTree.EnvTree(
                    configuration = Configuration(
                        specialNames = Configuration.SpecialNames(
                            specialNames = listOf(
                                Configuration.SpecialNames.SpecialName("DECIMAL-POINT", "COMMA")
                            )
                        )
                    )
                ), procedure = CobolFIRTree.ProcedureTree(
                    topLevel = listOf(
                        Display(StringLiteral("HELLO"))
                    )
                )
            ), input.toTree()
        )
    }

    @Test
    fun fileConfig() {
        val input = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 ENVIRONMENT DIVISION.
            123456* INPUT I
            123456* INPUT II
            123456 INPUT-OUTPUT SECTION.
            123456* FILE I
            123456* FILE II
            123456 FILE-CONTROL.
            123456* FOO I
            123456* FOO II
            123456     SELECT FOO-FILE ASSIGN FOO FILE STATUS FOO-STATUS.
            123456 PROCEDURE                   DIVISION.
            123456     DISPLAY "HELLO".
        """.trimIndent()

        assertEquals(
            CobolFIRTree(
                id = CobolFIRTree.ID(
                    programID = "HELLO"
                ), env = CobolFIRTree.EnvTree(
                    inputOutput = InputOutput(
                        fileControl = InputOutput.FileControl(
                            files = listOf(
                                InputOutput.FileControl.File(
                                    file = "FOO-FILE",
                                    fileVariable = "FOO",
                                    fileStatus = "FOO-STATUS",
                                    comments = listOf("FOO I", "FOO II")
                                )
                            ), comments = listOf("FILE I", "FILE II")
                        ), comments = listOf("INPUT I", "INPUT II")
                    )
                ), procedure = CobolFIRTree.ProcedureTree(
                    topLevel = listOf(
                        Display(StringLiteral("HELLO"))
                    )
                )
            ), input.toTree()
        )
    }

    @Test
    fun records() {
        //language=cobol
        val input = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 AUTHOR. WEDEMANN / Softwork.app
            123456 DATA DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 01 RPI.
            123456 05 FOO1.
            123456 10 WORLD2 PIC 9 VALUE 9.
            123456 15 WORLD3 POINTER.
            123456 20 WORLD4 PIC X(6) VALUE 'WORLD!' OCCURS 2.
            123456 25 FOO5 PIC 9(9) VALUE 123456 OCCURS 9 TO 9 DEPENDING ON WORLD2.
            123456 30      PIC 9(2).
            123456 77 WORLD6 PIC A.
            123456 77 FOO7 PIC S9(6)V9 VALUE .9.
            123456 77 FOO8 PIC V9 VALUE .9.
            123456 01 RPICA.
            123456    05 FOOPIC PIC 9(3).
            123456 77 FOO9 PIC 9(3).
            123456 PROCEDURE                   DIVISION.
            123456     DISPLAY "HELLO"WORLD4.
        """.trimIndent()
        val world4 =
            StringElementar("WORLD4", formatter = Simple(6), value = "WORLD!", occurs = Occurs(2))
        assertEquals(CobolFIRTree(id = CobolFIRTree.ID(
            programID = "HELLO", author = "WEDEMANN / Softwork.app"
        ), data = CobolFIRTree.DataTree(workingStorage = build {
            +Record("RPI") {
                +EmptyElementar("FOO1")
                val world2 = NumberElementar("WORLD2", formatter = Simple(1), value = 9.0)
                +world2
                +Pointer("WORLD3")
                +world4
                +NumberElementar(
                    "FOO5",
                    formatter = Simple(9),
                    value = 123456.0,
                    occurs = Occurs(9, 9, dependingOn = world2)
                )
            }

            +StringElementar("WORLD6", formatter = Simple(1))
            +NumberElementar(
                "FOO7",
                formatter = Custom(Signed(6), Decimal(1)),
                value = .9,
                signed = true
            )
            +NumberElementar("FOO8", formatter = Simple(1), value = .9, signed = true)
            +Record("RPICA") {
                +NumberElementar("FOOPIC", formatter = Simple(3))
            }
            +NumberElementar("FOO9", formatter = Simple(3))
        }), procedure = CobolFIRTree.ProcedureTree(topLevel = build {
            +Display(StringLiteral("HELLO") + StringVariable(world4))
        })
        ), input.toTree()
        )
    }
}
