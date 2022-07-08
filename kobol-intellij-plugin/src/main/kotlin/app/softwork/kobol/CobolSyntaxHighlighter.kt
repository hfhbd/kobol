package app.softwork.kobol

import app.softwork.kobol.CobolTypes.*
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
        COMMENT -> comment
        VARNAME -> vars
        DISPLAY_LITERAL -> function
        STRING -> string

        DIVISION, SECTION,
        IDENTIFICATION, PROGRAM_ID, AUTHOR, DATE, INSTALLATION,
        ENVIRONMENT,
        CONFIGURATION, SPECIAL_NAMES_LITERAL, IS,
        INPUT_OUTPUT_LITERAL, FILE_CONTROL_LITERAL, FILE_CONFIG_SELECT_LITERAL, FILE_CONFIG_ASSIGN_LITERAL, FILE_LITERAL, FILE_CONFIG_STATUS_STATUS_LITERAL,
        TO,

        WORKING_STORAGE,
        FILE_LITERAL, FD, RECORD_LITERAL, RECORDING, STANDARD, LABEL,

        DATA,
        PROCEDURE -> keyword

        TokenType.BAD_CHARACTER -> badCharacter
        DOT -> dot

        TokenType.WHITE_SPACE, ANY -> empty
        else -> empty
    }.also {
        if (it === empty) {
            println("$tokenType results: empty")
        } else {
            println("$tokenType results: ${it.single()}")
        }
    }
}
