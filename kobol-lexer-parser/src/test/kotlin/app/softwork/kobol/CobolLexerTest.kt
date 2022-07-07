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
        assertEquals(
            listOf(
                WHITE_SPACE,
                WHITE_SPACE,
                IDENTIFICATION,
                WHITE_SPACE,
                DIVISION,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                COMMENT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                PROGRAM_ID,
                DOT,
                WHITE_SPACE,
                VARNAME,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                AUTHOR,
                ANY,
                WHITE_SPACE,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                WHITE_SPACE,
                ANY,
                WHITE_SPACE,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                INSTALLATION,
                ANY,
                WHITE_SPACE,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                DATE,
                WHITE_SPACE,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY,
                ANY
            ), all
        )
    }

    @Test
    fun string() {
        val input = """
            123456 DISPLAY "HELLO"
            123456 DISPLAY 'WORLD'
            123456 MOVE "42" TO WORLD
            123456 MOVE '42' TO WORLD
            123456 DISPLAY 'WORLD'.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
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
            123456 DISPLAY "HELLO"WORLD
            123456 DISPLAY "HELLO"WORLD.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, VARNAME, DOT
            ),
            all
        )
    }

    @Test
    fun inner6Digits() {
        val input = """
            123456 MOVE 123456 TO HELLO
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, MOVE, WHITE_SPACE, NUMBER, WHITE_SPACE, TO, WHITE_SPACE, VARNAME
            ),
            all
        )
    }

    @Test
    fun workingSection() {
        val input = """
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 77 WORLD PIC A(6).
            123456 77 WORLD PIC A.
            123456 77 FOO PIC 9(6) VALUE 123456.
            123456 77 FOO PIC S9(6) VALUE 123456.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE,
                WHITE_SPACE,
                WORKING_STORAGE,
                WHITE_SPACE,
                SECTION,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                SA_LITERAL,
                WHITE_SPACE,
                VARNAME,
                WHITE_SPACE,
                PIC_LITERAL,
                WHITE_SPACE,
                PIC_XA,
                LP,
                NUMBER,
                RP,
                WHITE_SPACE,
                VALUE,
                WHITE_SPACE,
                STRING,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                SA_LITERAL,
                WHITE_SPACE,
                VARNAME,
                WHITE_SPACE,
                PIC_LITERAL,
                WHITE_SPACE,
                PIC_XA,
                LP,
                NUMBER,
                RP,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                SA_LITERAL,
                WHITE_SPACE,
                VARNAME,
                WHITE_SPACE,
                PIC_LITERAL,
                WHITE_SPACE,
                PIC_XA,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                SA_LITERAL,
                WHITE_SPACE,
                VARNAME,
                WHITE_SPACE,
                PIC_LITERAL,
                WHITE_SPACE,
                PIC_9,
                LP,
                NUMBER,
                RP,
                WHITE_SPACE,
                VALUE,
                WHITE_SPACE,
                NUMBER,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                SA_LITERAL,
                WHITE_SPACE,
                VARNAME,
                WHITE_SPACE,
                PIC_LITERAL,
                WHITE_SPACE,
                PIC_S,
                PIC_9,
                LP,
                NUMBER,
                RP,
                WHITE_SPACE,
                VALUE,
                WHITE_SPACE,
                NUMBER,
                DOT
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
            123456
            123456* Some Comment
            123456 DISPLAY
            123456******************************************************************
            123456 PROCEDURE.
            123456******************************************************************
            123456
            123456* Some Comment
            123456 DISPLAY
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, PROCEDURE, DOT, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE,
                WHITE_SPACE, COMMENT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY,
            ),
            all
        )
    }

    private fun FlexAdapter.all(input: String): Sequence<IElementType> = sequence {
        start(input)
        while (tokenType != null) {
            yield(tokenType!!)
            advance()
        }
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
                WHITE_SPACE,
                WHITE_SPACE,
                IDENTIFICATION,
                WHITE_SPACE,
                DIVISION,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                COMMENT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                PROGRAM_ID,
                DOT,
                WHITE_SPACE,
                VARNAME,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                COMMENT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                DATA,
                WHITE_SPACE,
                DIVISION,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                WORKING_STORAGE,
                WHITE_SPACE,
                SECTION,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                SA_LITERAL,
                WHITE_SPACE,
                VARNAME,
                WHITE_SPACE,
                PIC_LITERAL,
                WHITE_SPACE,
                PIC_XA,
                LP,
                NUMBER,
                RP,
                WHITE_SPACE,
                VALUE,
                WHITE_SPACE,
                STRING,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                COMMENT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                PROCEDURE,
                WHITE_SPACE,
                DIVISION,
                DOT,
                WHITE_SPACE,
                WHITE_SPACE,
                COMMENT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                COMMENT,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                DISPLAY,
                WHITE_SPACE,
                STRING,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                DISPLAY,
                WHITE_SPACE,
                STRING,
                VARNAME,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                MOVE,
                WHITE_SPACE,
                STRING,
                WHITE_SPACE,
                TO,
                WHITE_SPACE,
                VARNAME,
                WHITE_SPACE,
                WHITE_SPACE,
                WHITE_SPACE,
                DISPLAY,
                WHITE_SPACE,
                STRING,
                VARNAME,
                DOT
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
}
