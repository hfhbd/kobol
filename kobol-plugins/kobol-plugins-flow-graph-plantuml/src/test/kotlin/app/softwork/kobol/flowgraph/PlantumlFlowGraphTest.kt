package app.softwork.kobol.flowgraph

import app.softwork.kobol.fir.*
import org.intellij.lang.annotations.Language
import java.io.*
import kotlin.test.*

class PlantumlFlowGraphTest {
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
            123456* Some Comment
            123456     DISPLAY "HELLO"WORLD
            123456     IF WORLD = "WORLD" THEN
            123456     MOVE "42" TO WORLD
            123456     ELSE DISPLAY "ANSWER"WORLD
            123456     END-IF.
        """.trimIndent().toTree()
        //language=puml
        assertEquals("""
            @startuml
            start
            :DISPLAY "HELLO" WORLD;
            if (WORLD = "WORLD") then
            :MOVE "42" TO WORLD;
            else
            :DISPLAY "ANSWER" WORLD;
            endif
            end
            @enduml
            
        """.trimIndent(), cobol.createFlowGraph())
    }
}

internal fun String.toTree() =
    File.createTempFile("testing", ".cbl").apply { writeText(this@toTree) }.toTree()
