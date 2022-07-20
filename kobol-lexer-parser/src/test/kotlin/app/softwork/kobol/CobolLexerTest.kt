package app.softwork.kobol

import app.softwork.kobol.CobolTypes.*
import com.intellij.lexer.*
import com.intellij.psi.TokenType.*
import com.intellij.psi.tree.*
import kotlin.test.*

class CobolLexerTest {
    @Test
    fun id() {
        val input = """
            123456 IDENTIFICATION              DIVISION.
            123456/*****************************************************************
            123456 PROGRAM-ID.                 HELLO.
            123456 AUTHOR. WEDEMANN / Softwork.app
            123456 INSTALLATION. Softwork.app
            123456 DATE-WRITTEN TODAY.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(listOf(
            WHITE_SPACE, WHITE_SPACE, IDENTIFICATION, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
            WHITE_SPACE, WHITE_SPACE, COMMENT, WHITE_SPACE,
            WHITE_SPACE, WHITE_SPACE, PROGRAM_ID, DOT, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
            WHITE_SPACE, WHITE_SPACE, AUTHOR, ANY, WHITE_SPACE, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, WHITE_SPACE, ANY, WHITE_SPACE, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, WHITE_SPACE,
            WHITE_SPACE, WHITE_SPACE, INSTALLATION, ANY, WHITE_SPACE, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, WHITE_SPACE,
            WHITE_SPACE, WHITE_SPACE, DATE, WHITE_SPACE, ANY, ANY, ANY, ANY, ANY, ANY
        ), all)
    }

    @Test
    fun string() {
        val input = """
            123456 PROCEDURE DIVISION.
            123456 DISPLAY "HELLO"
            123456 DISPLAY 'WORLD'
            123456 MOVE "42" TO WORLD
            123456 MOVE '42' TO WORLD
            123456 DISPLAY 'WORLD'.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, PROCEDURE, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, MOVE, WHITE_SPACE, STRING, WHITE_SPACE, TO, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, MOVE, WHITE_SPACE, STRING, WHITE_SPACE, TO, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, DOT
            ),
            all
        )
    }

    @Test
    fun stringInterpolation() {
        val input = """
            123456 PROCEDURE DIVISION.
            123456 DISPLAY "HELLO" WORLD
            123456 DISPLAY "HELLO"WORLD.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, PROCEDURE, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, VARNAME, DOT
            ),
            all
        )
    }

    @Test
    fun inner6Digits() {
        val input = """
            123456 PROCEDURE DIVISION.
            123456 MOVE 123456 TO HELLO.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, PROCEDURE, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, MOVE, WHITE_SPACE, NUMBER, WHITE_SPACE, TO, WHITE_SPACE, VARNAME, DOT
            ),
            all
        )
    }

    @Test
    fun workingSection() {
        val input = """
            123456 DATA DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 01 RPI.
            123456 05 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 77 WORLD PIC A(6) OCCURS 1.
            123456 77 WORLD PIC A.
            123456 77 FOO PIC 9(9) VALUE 123456 OCCURS 9 TO 9 DEPENDING ON WORLD.
            123456 77 FOO PIC S9(6)V9 VALUE .9.
            123456 77 FOO PIC V9 VALUE .9.
            123456 01 RPICA.
            123456    05 FOOPIC PIC 9(3).
            123456 77 FOO PIC 9(3).
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, WORKING_STORAGE, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_XA, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, STRING, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_XA, LP, NUMBER, RP, WHITE_SPACE, OCCURS, WHITE_SPACE, NUMBER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_XA, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_9, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, NUMBER, WHITE_SPACE, OCCURS, WHITE_SPACE, NUMBER, WHITE_SPACE, TO, WHITE_SPACE, NUMBER, WHITE_SPACE, DEPENDING, WHITE_SPACE, ON, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_S_9, LP, NUMBER, RP, PIC_V_9, WHITE_SPACE, VALUE, WHITE_SPACE, NUMBER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_V_9, WHITE_SPACE, VALUE, WHITE_SPACE, NUMBER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_9, LP, NUMBER, RP, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_9, LP, NUMBER, RP, DOT
            ),
            all
        )
    }

    @Test
    fun data() {
        val all = CobolLexerAdapter.all(
            """
                123456 DATA DIVISION.
                123456 FILE SECTION.
                123456 FD FOO
                123456     RECORDING               V
                123456     LABEL RECORD            STANDARD
                123456     DATA RECORD             BAR.
                123456 01 BAR.
                123456    05 FOO PIC 9(3) COMP.
                123456    05 FOO PIC 9(3).            
                123456 WORKING-STORAGE SECTION.
                123456 01 RPI.
                123456 05 WORLD PIC X(6) VALUE 'WORLD!'.
                123456 77 WORLD PIC A(6).
                123456 77 WORLD PIC A.
                123456 01 FOO PIC 9(9) VALUE 123456.
                123456 77 FOO PIC S9(6) VALUE 123456.
                123456 01 RPICA.
                123456    05 FOOPIC PIC 9(3).
                123456 77 FOO PIC 9(3).
            """.trimIndent()
        ).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, FILE_LITERAL, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FD, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, RECORDING_LITERAL, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, LABEL_LITERAL, WHITE_SPACE, RECORD_LITERAL, WHITE_SPACE, STANDARD, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, RECORD_LITERAL, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_9, LP, NUMBER, RP, WHITE_SPACE, COMP, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_9, LP, NUMBER, RP, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, WORKING_STORAGE, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_XA, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, STRING, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_XA, LP, NUMBER, RP, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_XA, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_9, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, NUMBER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_S_9, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, NUMBER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_9, LP, NUMBER, RP, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_9, LP, NUMBER, RP, DOT
            ),
            all
        )
    }

    @Test
    fun emptyDataDivision() {
        val input = """
            123456 DATA DIVISION.
            123456 PROCEDURE DIVISION.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, PROCEDURE, WHITE_SPACE, DIVISION, DOT
            ),
            all
        )
    }

    @Test
    fun comment() {
        val input = """
            123456******************************************************************
            123456 PROCEDURE DIVISION.
            123456******************************************************************
            123456
            123456* Some Comment
            123456 DISPLAY
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, PROCEDURE, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY
            ),
            all
        )
    }

    @Test
    fun simple() {
        val input = """
            123456 IDENTIFICATION              DIVISION.
            123456******************************************************************
            123456 PROGRAM-ID.                 HELLO.
            123456******************************************************************
            123456 DATA                        DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456******************************************************************
            123456 PROCEDURE                   DIVISION.
            123456******************************************************************
            123456
            123456* Some Comment
            123456     DISPLAY "Foo"
            123456     DISPLAY "HELLO"WORLD
            123456     MOVE "42" TO WORLD
            123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, IDENTIFICATION, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, PROGRAM_ID, DOT, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, WORKING_STORAGE, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_XA, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, STRING, DOT, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, PROCEDURE, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, MOVE, WHITE_SPACE, STRING, WHITE_SPACE, TO, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, VARNAME, DOT
            ),
            all
        )
    }

    @Test
    fun procedureSection() {
        val input = """
            123456 PROCEDURE DIVISION.
            123456 FOO SECTION.
            123456* Some Comment
            123456     DISPLAY "Foo"
            123445     PERFORM BAR.
            123456 BAR SECTION.
            123456     DISPLAY "HELLO"WORLD.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, PROCEDURE, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, VARNAME, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, PERFORM, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, VARNAME, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, VARNAME, DOT
            ),
            all
        )
    }

    @Test
    fun specialNames() {
        val input = """
            123456 ENVIRONMENT DIVISION.
            123456 CONFIGURATION SECTION.
            123456 SPECIAL-NAMES.
            123456     DECIMAL-POINT           IS COMMA.
        """.trimIndent()

        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, ENVIRONMENT, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, CONFIGURATION, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, SPECIAL_NAMES_LITERAL, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, VARNAME, WHITE_SPACE, IS, WHITE_SPACE, VARNAME, DOT
            ),
            all
        )
    }

    @Test
    fun fileConfig() {
        val input = """
            123456 ENVIRONMENT DIVISION.
            123456 INPUT-OUTPUT SECTION.
            123456 FILE-CONTROL.
            123456     SELECT FOO-FILE ASSIGN FOO FILE STATUS FOO-STATUS.
            123456 DATA DIVISION.
            123456 FILE SECTION.
            123456 FD FOO
            123456 RECORDING V
        """.trimIndent()

        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, ENVIRONMENT, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, INPUT_OUTPUT_LITERAL, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FILE_CONTROL_LITERAL, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FILE_CONFIG_SELECT_LITERAL, WHITE_SPACE, VARNAME, WHITE_SPACE, FILE_CONFIG_ASSIGN_LITERAL, WHITE_SPACE, VARNAME, WHITE_SPACE, FILE_LITERAL, WHITE_SPACE, FILE_CONFIG_STATUS_STATUS_LITERAL, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FILE_LITERAL, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FD, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, RECORDING_LITERAL, WHITE_SPACE, VARNAME
            ),
            all
        )
    }

    @Test
    fun file() {
        //language=cobol
        val input = """
            123456 DATA DIVISION.
            123456 FILE SECTION.
            123456 FD FOO
            123456     RECORDING               V
            123456     LABEL RECORD            STANDARD
            123456     DATA RECORD             BAR.
            123456 01 BAR.
            123456    05 FOO PIC 9(3) COMP.
            123456    05 FOO PIC 9(3).
            123456 FD  FOO
            123456     RECORDING               F
            123456     BLOCK                   0
            123456     RECORD                  1
            123456     LABEL RECORD            STANDARD
            123456     DATA RECORD             BAR.
        """.trimIndent()

        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, FILE_LITERAL, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FD, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, RECORDING_LITERAL, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, LABEL_LITERAL, WHITE_SPACE, RECORD_LITERAL, WHITE_SPACE, STANDARD, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, RECORD_LITERAL, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_9, LP, NUMBER, RP, WHITE_SPACE, COMP, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC_LITERAL, WHITE_SPACE, PIC_9, LP, NUMBER, RP, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, FD, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, RECORDING_LITERAL, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, BLOCK_LITERAL, WHITE_SPACE, NUMBER, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, RECORD_LITERAL, WHITE_SPACE, NUMBER, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, LABEL_LITERAL, WHITE_SPACE, RECORD_LITERAL, WHITE_SPACE, STANDARD, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, RECORD_LITERAL, WHITE_SPACE, VARNAME, DOT
            ),
            all
        )
    }

    @Test
    fun sqlWorkingStorage() {
        val input = """
            123456 DATA DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 EXEC SQL FOO
            123456 BAR
            123456 END-EXEC.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, WORKING_STORAGE, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, EXEC, WHITE_SPACE, SQL, WHITE_SPACE, ANY, ANY, ANY, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, ANY, ANY, ANY, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, END_EXEC, DOT
            ),
            all
        )
    }

    private fun FlexAdapter.all(input: String): Sequence<IElementType> = sequence {
        start(input)
        while (true) {
            val tokenType = tokenType ?: break
            yield(tokenType)
            advance()
        }
    }
}
