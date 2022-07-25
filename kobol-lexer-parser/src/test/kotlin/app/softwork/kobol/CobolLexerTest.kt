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
            WHITE_SPACE, WHITE_SPACE, DATE_WRITTEN, WHITE_SPACE, ANY, ANY, ANY, ANY, ANY, ANY
        ), all)
    }

    @Test
    fun string() {
        val input = """
            123456 PROCEDURE DIVISION.
            123456 DISPLAY "HELLO"
            123456 DISPLAY 'WORLD'
            123456 MOVE "42" 
            123456 TO WORLD
            123456 MOVE '42' TO WORLD OF FOO
            123456 DISPLAY 'WORLD'.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, PROCEDURE, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, MOVE, WHITE_SPACE, STRING, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, TO, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, MOVE, WHITE_SPACE, STRING, WHITE_SPACE, TO, WHITE_SPACE, VARNAME, WHITE_SPACE, OF, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, DOT
            ),
            all
        )
    }

    @Test
    fun stringInterpolation() {
        val input = """
            123456 PROCEDURE DIVISION.
            123456 DISPLAY "HELLO" WORLD IN FOO
            123456 DISPLAY "HELLO"WORLD.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, PROCEDURE, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DISPLAY, WHITE_SPACE, STRING, WHITE_SPACE, VARNAME, WHITE_SPACE, IN, WHITE_SPACE, VARNAME, WHITE_SPACE,
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
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, X, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, STRING, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, A, LP, NUMBER, RP, WHITE_SPACE, OCCURS, WHITE_SPACE, NUMBER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, A, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, PIC9, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, NUMBER, WHITE_SPACE, OCCURS, WHITE_SPACE, NUMBER, WHITE_SPACE, TO, WHITE_SPACE, NUMBER, WHITE_SPACE, DEPENDING, WHITE_SPACE, ON, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, S9, LP, NUMBER, RP, V9, WHITE_SPACE, VALUE, WHITE_SPACE, NUMBER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, V9, WHITE_SPACE, VALUE, WHITE_SPACE, NUMBER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, PIC9, LP, NUMBER, RP, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, PIC9, LP, NUMBER, RP, DOT
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
                123456 77 WORLD REDEFINES FOO.
                123456 77 WORLD POINTER.
                123456 01 FOO PIC 9(9) VALUE 123456.
                123456 77 FOO PIC S9(6) VALUE 123456.
                123456 01 RPICA.
                123456    05 PIC 9(3).
                123456 77 FOO PIC 9(3).
            """.trimIndent()
        ).toList()
        assertEquals(
            listOf(
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, FILE, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FD, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, RECORDING, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, LABEL, WHITE_SPACE, RECORD, WHITE_SPACE, STANDARD, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, RECORD, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, PIC9, LP, NUMBER, RP, WHITE_SPACE, COMP, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, PIC9, LP, NUMBER, RP, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, WORKING_STORAGE, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, X, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, STRING, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, REDEFINES, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, POINTER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, PIC9, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, NUMBER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, S9, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, NUMBER, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, PIC, WHITE_SPACE, PIC9, LP, NUMBER, RP, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, PIC9, LP, NUMBER, RP, DOT
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
        //language=Cobol
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
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, X, LP, NUMBER, RP, WHITE_SPACE, VALUE, WHITE_SPACE, STRING, DOT, WHITE_SPACE,
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
                WHITE_SPACE, WHITE_SPACE, SPECIAL_NAMES, DOT, WHITE_SPACE,
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
                WHITE_SPACE, WHITE_SPACE, INPUT_OUTPUT, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FILE_CONTROL, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, SELECT, WHITE_SPACE, VARNAME, WHITE_SPACE, ASSIGN, WHITE_SPACE, VARNAME, WHITE_SPACE, FILE, WHITE_SPACE, STATUS, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FILE, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FD, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, RECORDING, WHITE_SPACE, VARNAME
            ),
            all
        )
    }

    @Test
    fun file() {
        val input = """
            123456 DATA DIVISION.
            123456 FILE SECTION.
            123456 FD FOO
            123456     RECORDING               V
            123456     LABEL RECORD            STANDARD
            123456     DATA RECORD             BAR.
            123456 01 BAR.
            123456    05 FOO PIC 9(3) USAGE COMP.
            123456    05 FOO PIC 9(3) COMP-3.
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

                WHITE_SPACE, WHITE_SPACE, FILE, WHITE_SPACE, SECTION, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, FD, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, RECORDING, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, LABEL, WHITE_SPACE, RECORD, WHITE_SPACE, STANDARD, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, RECORD, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, PIC9, LP, NUMBER, RP, WHITE_SPACE, USAGE, WHITE_SPACE, COMP, DOT, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, NUMBER, WHITE_SPACE, VARNAME, WHITE_SPACE, PIC, WHITE_SPACE, PIC9, LP, NUMBER, RP, WHITE_SPACE, COMP_3, DOT, WHITE_SPACE,

                WHITE_SPACE, WHITE_SPACE, FD, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, RECORDING, WHITE_SPACE, VARNAME, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, BLOCK, WHITE_SPACE, NUMBER, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, RECORD, WHITE_SPACE, NUMBER, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, LABEL, WHITE_SPACE, RECORD, WHITE_SPACE, STANDARD, WHITE_SPACE,
                WHITE_SPACE, WHITE_SPACE, DATA, WHITE_SPACE, RECORD, WHITE_SPACE, VARNAME, DOT
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

    @Test
    fun conditions() {
        val input = """
            123456 PROCEDURE DIVISION.
            123456 IF var(1:ff) NOT = bar
            123456     EXEC SQL FOO
            123456          BAR
            123456     END-EXEC
            123456 ELSE 
            123456     OPEN INPUT aa
            123456     READ aa
            123456       AT END 
            123456          CALL 'a' USING f
            123456     CLOSE a
            123456 END-IF
            123456 EVALUATE z
            123456     WHEN 1
            123456        ADD 1 TO a
            123456 END-EVALUATE
            123456 GOBACK.
        """.trimIndent()
        val all = CobolLexerAdapter.list(input)
        assertEquals(
            listOf(
                listOf(PROCEDURE, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE),
                listOf(IF, WHITE_SPACE, VARNAME, LP, NUMBER, COLON, VARNAME, RP, WHITE_SPACE, NOT, WHITE_SPACE, EQUAL, WHITE_SPACE, VARNAME, WHITE_SPACE),
                listOf(EXEC, WHITE_SPACE, SQL, WHITE_SPACE, ANY, ANY, ANY, WHITE_SPACE),
                listOf(ANY, ANY, ANY, WHITE_SPACE),
                listOf(END_EXEC, WHITE_SPACE),
                listOf(ELSE, WHITE_SPACE),
                listOf(OPEN, WHITE_SPACE, INPUT, WHITE_SPACE, VARNAME, WHITE_SPACE),
                listOf(READ, WHITE_SPACE, VARNAME, WHITE_SPACE),
                listOf(AT, WHITE_SPACE, END, WHITE_SPACE),
                listOf(CALL, WHITE_SPACE, STRING, WHITE_SPACE, USING, WHITE_SPACE, VARNAME, WHITE_SPACE),
                listOf(CLOSE, WHITE_SPACE, VARNAME, WHITE_SPACE),
                listOf(END_IF, WHITE_SPACE),
                listOf(EVALUATE, WHITE_SPACE, VARNAME, WHITE_SPACE),
                listOf(WHEN, WHITE_SPACE, NUMBER, WHITE_SPACE),
                listOf(ADD, WHITE_SPACE, NUMBER, WHITE_SPACE, TO, WHITE_SPACE, VARNAME, WHITE_SPACE),
                listOf(END_EVALUATE, WHITE_SPACE),
                listOf(GOBACK, DOT)
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

    private fun FlexAdapter.list(input: String): List<List<IElementType>> = buildList {
        var previous: IElementType? = null
        val list = mutableListOf<IElementType>()
        for (type in all(input)) {
            if (previous != null && type == WHITE_SPACE && previous == WHITE_SPACE) {
                add(list.toList())
                list.clear()
            } else {
                list.add(type)
            }
            previous = type
        }
        add(list)
    }.drop(1).filter { it.isNotEmpty() }
}
