package app.softwork.kobol

import com.intellij.openapi.editor.*
import com.intellij.openapi.editor.colors.*
import com.intellij.openapi.editor.colors.TextAttributesKey.*
import com.intellij.openapi.fileTypes.*
import com.intellij.psi.*
import com.intellij.psi.tree.*

object CobolSyntaxHighlighter : SyntaxHighlighterBase() {
    private val empty = arrayOf<TextAttributesKey>()
    private val comment =
        arrayOf(createTextAttributesKey("COBOL_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT))
    private val badCharacter = arrayOf(createTextAttributesKey("COBOL_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER))
    private val vars = arrayOf(createTextAttributesKey("COBOL_VAR", DefaultLanguageHighlighterColors.LOCAL_VARIABLE))
    private val function =
        arrayOf(createTextAttributesKey("COBOL_FUNCTIONS", DefaultLanguageHighlighterColors.FUNCTION_CALL))
    private val string = arrayOf(createTextAttributesKey("COBOL_STRING", DefaultLanguageHighlighterColors.STRING))
    private val keyword = arrayOf(createTextAttributesKey("COBOL_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD))

    private val dot = arrayOf(createTextAttributesKey("COBOL_DOT", DefaultLanguageHighlighterColors.DOT))

    override fun getHighlightingLexer() = CobolLexerAdapter

    override fun getTokenHighlights(tokenType: IElementType?) = when (tokenType) {
        CobolTypes.COMMENT -> comment
        CobolTypes.VARNAME -> vars
        CobolTypes.DISPLAY_LITERAL -> function
        CobolTypes.STRING -> string

        CobolTypes.DIVISION, CobolTypes.SECTION,
        CobolTypes.IDENTIFICATION, CobolTypes.PROGRAM_ID, CobolTypes.AUTHOR, CobolTypes.DATE, CobolTypes.INSTALLATION,
        CobolTypes.ENVIRONMENT,
        CobolTypes.CONFIGURATION, CobolTypes.SPECIAL_NAMES_LITERAL, CobolTypes.IS,
        CobolTypes.INPUT_OUTPUT_LITERAL, CobolTypes.FILE_CONTROL_LITERAL, CobolTypes.FILE_CONFIG_SELECT_LITERAL, CobolTypes.FILE_CONFIG_ASSIGN_LITERAL, CobolTypes.FILE_LITERAL, CobolTypes.FILE_CONFIG_STATUS_STATUS_LITERAL,
        CobolTypes.TO,

        CobolTypes.WORKING_STORAGE,

        CobolTypes.DATA,
        CobolTypes.PROCEDURE -> keyword

        TokenType.BAD_CHARACTER -> badCharacter
        CobolTypes.DOT -> dot

        TokenType.WHITE_SPACE, CobolTypes.ANY -> empty
        else -> empty
    }.also {
        if (it === empty) {
            println("$tokenType results: empty")
        } else {
            println("$tokenType results: ${it.single()}")
        }
    }
}
