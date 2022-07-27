package app.softwork.kobol

import kotlin.test.*

class CobolElementFactoryTest {
    @Test
    fun create() {
        val oldFile = """
            123456 IDENTIFICATION DIVISION.
            123456 PROGRAM-ID. HELLO.
            123456 PROCEDURE DIVISION.
            123456 DISPLAY FOO.
        """.trimIndent().toCobolFile()
        val new = CobolElementFactory.createVarName(oldFile.project, "TESTING")
        assertEquals(new.text, "TESTING")
    }
}
