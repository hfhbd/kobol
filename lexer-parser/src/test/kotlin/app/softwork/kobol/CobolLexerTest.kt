package app.softwork.kobol

import app.softwork.kobol.CobolTypes.*
import app.softwork.kobol.fir.*
import com.intellij.lexer.*
import com.intellij.psi.tree.*
import org.intellij.lang.annotations.*
import kotlin.test.*
import com.intellij.psi.TokenType.WHITE_SPACE as sp

class CobolLexerTest {
    @Test
    fun author() {
        assertEquals(
            """
            123456 IDENTIFICATION DIVISION.
            123456 PROGRAM-ID. BS12A00.
            123456 AUTHOR. A.
            123456 PROCEDURE DIVISION.
            123456 GOBACK.
            """.trimIndent(),
        ) {
            line(IDENTIFICATION, sp, DIVISION, DOT, sp)
            line(PROGRAM_ID, DOT, sp, VARNAME, DOT, sp)
            line(AUTHOR, ANY, sp, ANY, ANY, sp)
            line(PROCEDURE, sp, DIVISION, DOT, sp)
            line(GOBACK, DOT)
        }
    }

    @Test
    fun id() {
        assertEquals(
            """
            123456 IDENTIFICATION              DIVISION.
            123456/*****************************************************************
            123456 PROGRAM-ID.                 HELLO.
            123456 AUTHOR. WEDEMANN / Softwork.app
            123456 INSTALLATION. Softwork.app
            123456 DATE-WRITTEN TODAY.
            """.trimIndent(),
        ) {
            line(IDENTIFICATION, sp, DIVISION, DOT, sp)
            line(COMMENT, sp)
            line(PROGRAM_ID, DOT, sp, VARNAME, DOT, sp)
            line(AUTHOR, ANY, sp, *ANY * 8, sp, ANY, sp, *ANY * 12, sp)
            line(INSTALLATION, ANY, sp, *ANY * 12, sp)
            line(DATE_WRITTEN, sp, *ANY * 6)
        }
    }

    @Test
    fun string() {
        assertEquals(
            """
            123456 PROCEDURE DIVISION.
            123456 DISPLAY "HELLO"
            123456 DISPLAY 'WORLD'
            123456 MOVE "42" 
            123456 TO WORLD
            123456 MOVE '42' TO WORLD OF FOO
            123456 DISPLAY 'WORLD'.
            """.trimIndent(),
        ) {
            line(PROCEDURE, sp, DIVISION, DOT, sp)
            line(DISPLAY, sp, STRING, sp)
            line(DISPLAY, sp, STRING, sp)
            line(MOVE, sp, STRING, sp)
            line(TO, sp, VARNAME, sp)
            line(MOVE, sp, STRING, sp, TO, sp, VARNAME, sp, OF, sp, VARNAME, sp)
            line(DISPLAY, sp, STRING, DOT)
        }
    }

    @Test
    fun stringInterpolation() {
        assertEquals(
            """
            123456 PROCEDURE DIVISION.
            123456 DISPLAY "HELLO" WORLD IN FOO
            123456 DISPLAY "HELLO"WORLD.
            """.trimIndent(),
        ) {
            line(PROCEDURE, sp, DIVISION, DOT, sp)
            line(DISPLAY, sp, STRING, sp, VARNAME, sp, IN, sp, VARNAME, sp)
            line(DISPLAY, sp, STRING, VARNAME, DOT)
        }
    }

    @Test
    fun inner6Digits() {
        assertEquals(
            """
            123456 PROCEDURE DIVISION.
            123456 MOVE 123456 TO HELLO.
            """.trimIndent(),
        ) {
            line(PROCEDURE, sp, DIVISION, DOT, sp)
            line(MOVE, sp, NUMBER, sp, TO, sp, VARNAME, DOT)
        }
    }

    @Test
    fun workingSection() {
        assertEquals(
            """
            123456 DATA DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 01 RPI.
            123456 05 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 77 WORLD PIC A(6) OCCURS 1.
            123456 77 WORLD PIC A.
            123456 77 FOO PIC 9(9) VALUE 123456 
            123456    OCCURS 9 TO 9 DEPENDING ON WORLD.
            123456 77 FOO PIC S9(6)V9 VALUE .9.
            123456 77 FOO PIC V9 VALUE .9.
            123456 01 RPICA.
            123456    05 FOOPIC PIC 9(3).
            123456 77 FOO PIC 9(3).
            """.trimIndent(),
        ) {
            line(DATA, sp, DIVISION, DOT, sp)
            line(WORKING_STORAGE, sp, SECTION, DOT, sp)
            line(NUMBER, sp, VARNAME, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, X, LP, NUMBER, RP, sp, VALUE, sp, STRING, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, A, LP, NUMBER, RP, sp, OCCURS, sp, NUMBER, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, A, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, NUMBER, LP, NUMBER, RP, sp, VALUE, sp, NUMBER, sp)
            line(OCCURS, sp, NUMBER, sp, TO, sp, NUMBER, sp, DEPENDING, sp, ON, sp, VARNAME, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, S9, LP, NUMBER, RP, V9, sp, VALUE, sp, NUMBER, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, V9, sp, VALUE, sp, NUMBER, DOT, sp)
            line(NUMBER, sp, VARNAME, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, NUMBER, LP, NUMBER, RP, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, NUMBER, LP, NUMBER, RP, DOT)
        }
    }

    @Test
    fun data() {
        assertEquals(
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
            """.trimIndent(),
        ) {
            line(DATA, sp, DIVISION, DOT, sp)
            line(FILE, sp, SECTION, DOT, sp)

            line(FD, sp, VARNAME, sp)
            line(RECORDING, sp, VARNAME, sp)
            line(LABEL, sp, RECORD, sp, STANDARD, sp)
            line(DATA, sp, RECORD, sp, VARNAME, DOT, sp)

            line(NUMBER, sp, VARNAME, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, NUMBER, LP, NUMBER, RP, sp, COMP, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, NUMBER, LP, NUMBER, RP, DOT, sp)

            line(WORKING_STORAGE, sp, SECTION, DOT, sp)
            line(NUMBER, sp, VARNAME, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, X, LP, NUMBER, RP, sp, VALUE, sp, STRING, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, REDEFINES, sp, VARNAME, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, POINTER, DOT, sp)

            line(NUMBER, sp, VARNAME, sp, PIC, sp, NUMBER, LP, NUMBER, RP, sp, VALUE, sp, NUMBER, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, S9, LP, NUMBER, RP, sp, VALUE, sp, NUMBER, DOT, sp)
            line(NUMBER, sp, VARNAME, DOT, sp)
            line(NUMBER, sp, PIC, sp, NUMBER, LP, NUMBER, RP, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, NUMBER, LP, NUMBER, RP, DOT)
        }
    }

    @Test
    fun emptyDataDivision() {
        assertEquals(
            """
            123456 DATA DIVISION.
            123456 PROCEDURE DIVISION.
            """.trimIndent(),
        ) {
            line(DATA, sp, DIVISION, DOT, sp)
            line(PROCEDURE, sp, DIVISION, DOT)
        }
    }

    @Test
    fun comment() {
        assertEquals(
            """
            123456******************************************************************
            123456 PROCEDURE DIVISION.
            123456******************************************************************
            123456
            123456* Some Comment
            123456 DISPLAY
            """.trimIndent(),
        ) {
            line(COMMENT, sp)
            line(PROCEDURE, sp, DIVISION, DOT, sp)
            line(COMMENT, sp)
            line(COMMENT, sp)
            line(DISPLAY)
        }
    }

    @Test
    fun simple() {
        //language=Cobol
        assertEquals(
            """
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
            """.trimIndent(),
        ) {
            line(IDENTIFICATION, sp, DIVISION, DOT, sp)
            line(COMMENT, sp)
            line(PROGRAM_ID, DOT, sp, VARNAME, DOT, sp)
            line(COMMENT, sp)
            line(DATA, sp, DIVISION, DOT, sp)
            line(WORKING_STORAGE, sp, SECTION, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, X, LP, NUMBER, RP, sp, VALUE, sp, STRING, DOT, sp)
            line(COMMENT, sp)
            line(PROCEDURE, sp, DIVISION, DOT, sp)
            line(COMMENT, sp)
            line(COMMENT, sp)
            line(DISPLAY, sp, STRING, sp)
            line(DISPLAY, sp, STRING, VARNAME, sp)
            line(MOVE, sp, STRING, sp, TO, sp, VARNAME, sp)
            line(DISPLAY, sp, STRING, VARNAME, DOT)
        }
    }

    @Test
    fun procedureSection() {
        assertEquals(
            """
            123456 PROCEDURE DIVISION.
            123456 FOO SECTION.
            123456* Some Comment
            123456     DISPLAY "Foo"
            123445     PERFORM BAR.
            123456 BAR SECTION.
            123456     DISPLAY "HELLO"WORLD.
            """.trimIndent(),
        ) {
            line(PROCEDURE, sp, DIVISION, DOT, sp)
            line(VARNAME, sp, SECTION, DOT, sp)
            line(COMMENT, sp)
            line(DISPLAY, sp, STRING, sp)
            line(PERFORM, sp, VARNAME, DOT, sp)
            line(VARNAME, sp, SECTION, DOT, sp)
            line(DISPLAY, sp, STRING, VARNAME, DOT)
        }
    }

    @Test
    fun specialNames() {
        assertEquals(
            """
            123456 ENVIRONMENT DIVISION.
            123456 CONFIGURATION SECTION.
            123456 SPECIAL-NAMES.
            123456     DECIMAL-POINT           IS COMMA.
            """.trimIndent(),
        ) {
            line(ENVIRONMENT, sp, DIVISION, DOT, sp)
            line(CONFIGURATION, sp, SECTION, DOT, sp)
            line(SPECIAL_NAMES, DOT, sp)
            line(VARNAME, sp, IS, sp, VARNAME, DOT)
        }
    }

    @Test
    fun linkage() {
        assertEquals(
            """
            123456 DATA DIVISION.
            123456 LINKAGE SECTION.
            123456 01 FOO.
            123456 05 BAR PIC X.
            123456 PROCEDURE DIVISION USING FOO.
            """.trimIndent(),
        ) {
            line(DATA, sp, DIVISION, DOT, sp)
            line(LINKAGE, sp, SECTION, DOT, sp)
            line(NUMBER, sp, VARNAME, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, X, DOT, sp)
            line(PROCEDURE, sp, DIVISION, sp, USING, sp, VARNAME, DOT)
        }
    }

    @Test
    fun fileConfig() {
        assertEquals(
            """
            123456 ENVIRONMENT DIVISION.
            123456 INPUT-OUTPUT SECTION.
            123456 FILE-CONTROL.
            123456     SELECT FOO-FILE ASSIGN FOO FILE STATUS FOO-STATUS.
            123456 DATA DIVISION.
            123456 FILE SECTION.
            123456 FD FOO
            123456 RECORDING V
            """.trimIndent(),
        ) {
            line(ENVIRONMENT, sp, DIVISION, DOT, sp)
            line(INPUT_OUTPUT, sp, SECTION, DOT, sp)
            line(FILE_CONTROL, DOT, sp)
            line(SELECT, sp, VARNAME, sp, ASSIGN, sp, VARNAME, sp, FILE, sp, STATUS, sp, VARNAME, DOT, sp)
            line(DATA, sp, DIVISION, DOT, sp)
            line(FILE, sp, SECTION, DOT, sp)
            line(FD, sp, VARNAME, sp)
            line(RECORDING, sp, VARNAME)
        }
    }

    @Test
    fun file() {
        assertEquals(
            """
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
            """.trimIndent(),
        ) {
            line(DATA, sp, DIVISION, DOT, sp)
            line(FILE, sp, SECTION, DOT, sp)
            line(FD, sp, VARNAME, sp)
            line(RECORDING, sp, VARNAME, sp)
            line(LABEL, sp, RECORD, sp, STANDARD, sp)
            line(DATA, sp, RECORD, sp, VARNAME, DOT, sp)
            line(NUMBER, sp, VARNAME, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, NUMBER, LP, NUMBER, RP, sp, USAGE, sp, COMP, DOT, sp)
            line(NUMBER, sp, VARNAME, sp, PIC, sp, NUMBER, LP, NUMBER, RP, sp, COMP_3, DOT, sp)
            line(FD, sp, VARNAME, sp)
            line(RECORDING, sp, VARNAME, sp)
            line(BLOCK, sp, NUMBER, sp)
            line(RECORD, sp, NUMBER, sp)
            line(LABEL, sp, RECORD, sp, STANDARD, sp)
            line(DATA, sp, RECORD, sp, VARNAME, DOT)
        }
    }

    @Test
    fun sqlWorkingStorage() {
        assertEquals(
            """
            123456 DATA DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 EXEC SQL FOO
            123456 BAR BAR
            123456  BAR BAR
            123456 END-EXEC.
            """.trimIndent(),
        ) {
            line(DATA, sp, DIVISION, DOT, sp)
            line(WORKING_STORAGE, sp, SECTION, DOT, sp)
            line(EXEC, sp, SQL, SP, ANY, ANY, ANY, SP)
            line(ANY, ANY, ANY, SP, ANY, ANY, ANY, SP)
            line(ANY, ANY, ANY, SP, ANY, ANY, ANY, SP)
            line(END_EXEC, DOT)
        }
    }

    @Test
    fun conditions() {
        assertEquals(
            """
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
            123456       WRITE aa
            123456     CLOSE a
            123456 END-IF
            123456 EVALUATE z
            123456     WHEN 1
            123456        ADD 1 TO a
            123456 END-EVALUATE
            123456 IF FOO LESS THAN B
            123456 END-IF
            123456 GOBACK.
            """.trimIndent(),
        ) {
            line(PROCEDURE, sp, DIVISION, DOT, sp)
            line(IF, sp, VARNAME, LP, NUMBER, COLON, VARNAME, RP, sp, NOT, sp, EQUAL, sp, VARNAME, sp)
            line(EXEC, sp, SQL, SP, ANY, ANY, ANY, SP)
            line(ANY, ANY, ANY, SP)
            line(END_EXEC, sp)
            line(ELSE, sp)
            line(OPEN, sp, INPUT, sp, VARNAME, sp)
            line(READ, sp, VARNAME, sp)
            line(AT, sp, END, sp)
            line(CALL, sp, STRING, sp, USING, sp, VARNAME, sp)
            line(WRITE, sp, VARNAME, sp)
            line(CLOSE, sp, VARNAME, sp)
            line(END_IF, sp)
            line(EVALUATE, sp, VARNAME, sp)
            line(WHEN, sp, NUMBER, sp)
            line(ADD, sp, NUMBER, sp, TO, sp, VARNAME, sp)
            line(END_EVALUATE, sp)
            line(IF, sp, VARNAME, sp, SMALLER, sp, THAN, sp, VARNAME, sp)
            line(END_IF, sp)
            line(GOBACK, DOT)
        }
    }
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
        if (previous != null && (type == sp || type == SP) && (previous == sp || previous == SP)) {
            add(list.toList())
            list.clear()
        } else if (previous != null && type == COMMENT && (previous == sp || previous == SP)) {
            add(list.toList())
            list.clear()
            list.add(type)
        } else {
            list.add(type)
        }
        previous = type
    }
    add(list)
}.drop(1).filter { it.isNotEmpty() }

private fun assertEquals(@Language("COBOL") cobol: String, builder: Builder<List<IElementType>>.() -> Unit) {
    assertEquals(build(builder), CobolLexerAdapter().list(cobol))
}

private fun Builder<List<IElementType>>.line(vararg types: IElementType) = +listOf(elements = types)

private operator fun IElementType.times(times: Int) = Array(times) { this }
