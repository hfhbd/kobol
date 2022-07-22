package app.softwork.kobol

import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Elementar.*
import app.softwork.kobol.CobolFIRTree.DataTree.WorkingStorage.Record
import app.softwork.kobol.CobolFIRTree.EnvTree.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Expression.StringExpression.*
import app.softwork.kobol.CobolFIRTree.ProcedureTree.Statement.*
import com.intellij.psi.*
import kotlin.test.*

class CobolParserTest {
    @Test
    fun data() {
        assertEquals(
            CobolFIRTree(
                id = CobolFIRTree.ID(
                    programID = "HELLO"
                ),
                data = CobolFIRTree.DataTree(
                    workingStorage = listOf(
                        Record(
                            name = "RPI",
                            elements = listOf(
                                StringElementar(name = "WORLD", value = "WORLD!", formatter = Formatter.Simple(6)),
                                StringElementar(name = "ANSWER", formatter = Formatter.Simple(  6))
                            )
                        ),
                        StringElementar(name = "FOO", value = "123456", formatter = Formatter.Simple(6)),
                        Record(
                            name = "RPICA",
                            elements = listOf(
                                StringElementar(name = "FOOPIC", formatter = Formatter.Simple(3))
                            )
                        ),
                        StringElementar(name = "BAR", formatter = Formatter.Simple(3))
                    )
                ),
                procedure = CobolFIRTree.ProcedureTree(
                    topLevel = listOf(
                        Display(
                            StringVariable(target = StringElementar(name = "WORLD", value = "WORLD!", formatter = Formatter.Simple(6)))
                        )
                    )
                )
            ), """
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
                123456    05 FOOPIC PIC A(3).
                123456 77 BAR PIC A(3).
                123456 PROCEDURE DIVISION.
                123456 DISPLAY WORLD.
            """.trimIndent().toTree()
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
                ),
                data = CobolFIRTree.DataTree(
                    workingStorage = listOf(
                        StringElementar(name = "WORLD", value = "WORLD!", formatter = Formatter.Simple(6))
                    )
                ),
                procedure = CobolFIRTree.ProcedureTree(
                    topLevel = listOf(
                        Display(
                            Concat(
                                StringLiteral("HELLO"),
                                StringVariable(target = StringElementar(name = "WORLD", value = "WORLD!", formatter = Formatter.Simple(6)))
                            ),
                            comments = listOf("Some Comment")
                        ),
                        Move(
                            value = StringLiteral("42"),
                            target = StringElementar(name = "WORLD", value = "WORLD!", formatter = Formatter.Simple(6))
                        ),
                        Display(
                            Concat(
                                StringLiteral("ANSWER"),
                                StringVariable(target = StringElementar(name = "WORLD", value = "WORLD!", formatter = Formatter.Simple(6)))
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
                ),
                env = CobolFIRTree.EnvTree(
                    configuration = Configuration(
                        specialNames = Configuration.SpecialNames(
                            specialNames = listOf(
                                Configuration.SpecialNames.SpecialName("DECIMAL-POINT", "COMMA")
                            )
                        )
                    )
                ),
                procedure = CobolFIRTree.ProcedureTree(
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
                ),
                env = CobolFIRTree.EnvTree(
                    inputOutput = InputOutput(
                        fileControl = InputOutput.FileControl(
                            files = listOf(
                                InputOutput.FileControl.File(
                                    file = "FOO-FILE",
                                    fileVariable = "FOO",
                                    fileStatus = "FOO-STATUS",
                                    comments = listOf("FOO I", "FOO II")
                                )
                            ),
                            comments = listOf("FILE I", "FILE II")
                        ),
                        comments = listOf("INPUT I", "INPUT II")
                    )
                ),
                procedure = CobolFIRTree.ProcedureTree(
                    topLevel = listOf(
                        Display(StringLiteral("HELLO"))
                    )
                )
            ), input.toTree()
        )
    }

    @Test
    fun length() {
        //language=cobol
        val text = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 AUTHOR. WEDEMANN / Softwork.app
            123456 INSTALLATION. Softwork.app
            123456 DATE-WRITTEN TODAY.
            123456 DATA                        DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 PROCEDURE                   DIVISION.
            123456
            123456* Some Comment
            123456     DISPLAY "HELLO"WORLD
            123456     MOVE "42" TO WORLD
            123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
        val length = text.length
        assertEquals(473, length)
        val file = text.toCobolFile()
        assertEquals(473, file.textLength)
        assertEquals(473, file.node.textLength)
        assertEquals(473, file.psiRoots.single().node.textLength)
    }
}
