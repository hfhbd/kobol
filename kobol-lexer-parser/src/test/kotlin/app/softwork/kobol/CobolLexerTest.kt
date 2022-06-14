package app.softwork.kobol

import app.softwork.kobol.CobolTypes.*
import com.intellij.lexer.*
import com.intellij.psi.TokenType.*
import com.intellij.psi.tree.*
import kotlin.test.*

class CobolLexerTest {
    @Test
    fun a() {
        val input = """
            123456 IDENTIFICATION              DIVISION.
            123456******************************************************************
            123456 PROGRAM-ID.                 HELLO.
            123456 AUTHOR. WEDEMANN / Softwork.app
            123456 INSTALLATION. Softwork.app
            123456 DATE-WRITTEN TODAY.
        """.trimIndent()
        val all = CobolLexerAdapter.all(input).toList()
        assertEquals(listOf(
            WHITE_SPACE, WHITE_SPACE, IDENTIFICATION, WHITE_SPACE, DIVISION, DOT, WHITE_SPACE,
            WHITE_SPACE, COMMENT, WHITE_SPACE,
            WHITE_SPACE, WHITE_SPACE, PROGRAM_ID, DOT, WHITE_SPACE, VARNAME, DOT, WHITE_SPACE,
            WHITE_SPACE, WHITE_SPACE, AUTHOR, ANY, WHITE_SPACE, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, WHITE_SPACE, ANY, WHITE_SPACE, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, WHITE_SPACE,
            WHITE_SPACE, WHITE_SPACE, INSTALLATION, ANY, WHITE_SPACE, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY, WHITE_SPACE,
            WHITE_SPACE, WHITE_SPACE, DATE, WHITE_SPACE, ANY, ANY, ANY, ANY, ANY, ANY
        ), all)
    }

    private fun FlexAdapter.all(input: String): Sequence<IElementType> = sequence {
        start(input)
        while (tokenType != null) {
            yield(tokenType!!)
            advance()
        }
    }
}
