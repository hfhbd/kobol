package app.softwork.kobol.plugins

import app.softwork.kobol.fir.*
import app.softwork.kobol.*
import org.intellij.lang.annotations.Language
import java.io.*
import java.nio.file.Files
import kotlin.test.*

class StatisticTest {
    @Test
    fun simple() {
        @Language("Cobol")
        val cobol = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 DATA                        DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 PROCEDURE                   DIVISION.
            123456     IF WORLD = "WORLD" THEN
            123456       MOVE "42" TO WORLD
            123456     ELSE
            123456       DISPLAY "ANSWER"WORLD
            123456     END-IF.
        """.trimIndent().toTree()

        assertEquals(mapOf("main" to 2), cobol.complexity())
    }

    @Test
    fun ifAnd() {
        @Language("Cobol")
        val cobol = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 DATA                        DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 PROCEDURE                   DIVISION.
            123456     IF WORLD = "WORLD" AND WORLD = "WORLD" THEN
            123456       MOVE "42" TO WORLD
            123456     ELSE
            123456       DISPLAY "ANSWER"WORLD
            123456     END-IF.
        """.trimIndent().toTree()

        assertEquals(mapOf("main" to 3), cobol.complexity())
    }

    @Test
    fun complex() {
        @Language("Cobol")
        val cobol = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 DATA                        DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 PROCEDURE                   DIVISION.
            123456     IF WORLD = "WORLD" AND WORLD = "WORLD" THEN
            123456       MOVE "42" TO WORLD
            123456     ELSE
            123456       DISPLAY "ANSWER"WORLD
            123456     END-IF
            123456
            123456     EVALUATE WORLD
            123456     WHEN "WORLD"
            123456       DISPLAY "ANSWER"WORLD
            123456     END-EVALUATE.
        """.trimIndent().toTree()

        assertEquals(mapOf("main" to 4), cobol.complexity())
    }

    @Test
    fun alsoEvaluate() {
        @Language("Cobol")
        val cobol = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 DATA                        DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 PROCEDURE                   DIVISION.
            123456     EVALUATE WORLD ALSO WORLD
            123456     WHEN "WORLD" ALSO "WORLD"
            123456       DISPLAY "ANSWER"WORLD
            123456     WHEN "WORLD" ALSO "WORLD"
            123456       DISPLAY "ANSWER"WORLD
            123456     END-EVALUATE.
        """.trimIndent().toTree()

        assertEquals(mapOf("main" to 5), cobol.complexity())
    }

    @Test
    fun subroutines() {
        @Language("Cobol")
        val cobol = """
            123456 IDENTIFICATION              DIVISION.
            123456 PROGRAM-ID.                 HELLO.
            123456 DATA                        DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456 PROCEDURE                   DIVISION.
            123456     IF WORLD = "WORLD" AND WORLD = "WORLD" THEN
            123456       MOVE "42" TO WORLD
            123456     ELSE
            123456       DISPLAY "ANSWER"WORLD
            123456     END-IF
            123456
            123456     EVALUATE WORLD
            123456     WHEN "WORLD"
            123456       DISPLAY "ANSWER"WORLD
            123456     END-EVALUATE
            123456     PERFORM FOO.
            123456
            123456 FOO SECTION.
            123456     IF WORLD = "WORLD" AND WORLD = "WORLD" THEN
            123456       MOVE "42" TO WORLD
            123456     ELSE
            123456       DISPLAY "ANSWER"WORLD
            123456     END-IF
            123456
            123456     EVALUATE WORLD ALSO WORLD
            123456     WHEN "WORLD" ALSO "WORLD"
            123456       DISPLAY "ANSWER"WORLD
            123456     END-EVALUATE.
        """.trimIndent().toTree()

        assertEquals(mapOf("main" to 4, "FOO" to 5), cobol.complexity())
    }
}

internal fun String.toTree(): CobolFIRTree {
    val temp = Files.createTempDirectory("testing")
    return File(temp.toFile(), "testing.cbl").apply { writeText(this@toTree) }.toTree(temp)
}
