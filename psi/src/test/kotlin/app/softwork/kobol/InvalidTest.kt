package app.softwork.kobol

import app.softwork.kobol.fir.*
import java.io.*
import java.nio.file.*
import kotlin.test.*

class InvalidTest {
    @Test
    fun elementarNotFound() {
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 DATA DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 HELLO PIC A(6).
        123456 PROCEDURE                   DIVISION.
        123456     DISPLAY "HELLO"WORLD
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()

        val error = assertFailsWith<IllegalStateException> {
            input.toTree()
        }
        val message = assertNotNull(error.message)
        assertEquals("Elementar WORLD not found", message)
    }
}

internal fun String.toTree(vararg including: Pair<String, String>): CobolFIRTree {
    val tempFolder = Files.createTempDirectory("cobolTesting")
    val files = including.map { (name, content) ->
        File(tempFolder.toFile(), name).apply { writeText(content) }
    }

    return (
        files + File(tempFolder.toFile(), "testing.cbl").apply {
            writeText(this@toTree)
        }
        ).toCobolFile().single().toTree(tempFolder)
}

internal fun String.toCobolFile() =
    File.createTempFile("testing", ".cbl").apply { writeText(this@toCobolFile) }.toCobolFile()
