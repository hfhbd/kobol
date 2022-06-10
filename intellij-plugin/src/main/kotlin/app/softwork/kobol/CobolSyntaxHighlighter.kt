package app.softwork.kobol

import com.intellij.openapi.editor.*
import com.intellij.openapi.editor.colors.*
import com.intellij.openapi.editor.colors.TextAttributesKey.*
import com.intellij.openapi.fileTypes.*
import com.intellij.psi.*
import com.intellij.psi.tree.*

object CobolSyntaxHighlighter : SyntaxHighlighterBase() {
    private val COMMENT =
        createTextAttributesKey("COBOL_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT)
    private val VAR =
        createTextAttributesKey("COBOL_VAR", DefaultLanguageHighlighterColors.LOCAL_VARIABLE)
    private val FUNCTIONS = createTextAttributesKey("COBOL_FUNCTIONS", DefaultLanguageHighlighterColors.FUNCTION_CALL)
    private val BAD_CHARACTER = createTextAttributesKey("COBOL_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER)
    private val STRING = createTextAttributesKey("COBOL_STRING", DefaultLanguageHighlighterColors.STRING)

    private val EMPTY_KEYS = arrayOf<TextAttributesKey>()
    private val COMMENT_KEYS = arrayOf(COMMENT)
    private val BAD_CHARACTER_KEYS = arrayOf(BAD_CHARACTER)
    private val VAR_KEYS = arrayOf(VAR)
    private val functionKeys = arrayOf(FUNCTIONS)
    private val stringKeys = arrayOf(STRING)

    override fun getHighlightingLexer() = CobolLexerAdapter

    override fun getTokenHighlights(tokenType: IElementType?) = when (tokenType) {
        CobolTypes.COMMENT -> COMMENT_KEYS
        CobolTypes.VARNAME -> VAR_KEYS
        CobolTypes.DISPLAY -> functionKeys
        CobolTypes.STRING -> stringKeys
        TokenType.BAD_CHARACTER -> BAD_CHARACTER_KEYS
        else -> EMPTY_KEYS
    }
}
