package app.softwork.kobol.plugins

import app.softwork.kobol.fir.*
import org.intellij.lang.annotations.Language
import java.io.*
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
}

internal fun String.toTree() =
    File.createTempFile("testing", ".cbl").apply { writeText(this@toTree) }.toTree()
