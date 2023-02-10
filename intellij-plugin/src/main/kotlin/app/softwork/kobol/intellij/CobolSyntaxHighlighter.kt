package app.softwork.kobol.intellij

import app.softwork.kobol.*
import com.intellij.openapi.editor.*
import com.intellij.openapi.editor.colors.*
import com.intellij.openapi.fileTypes.*
import com.intellij.psi.*
import com.intellij.psi.tree.*

internal object CobolSyntaxHighlighter : SyntaxHighlighterBase() {
    private val comment =
        TextAttributesKey.createTextAttributesKey("COBOL_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT)
    private val badCharacter =
        TextAttributesKey.createTextAttributesKey("COBOL_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER)
    private val vars =
        TextAttributesKey.createTextAttributesKey("COBOL_VAR", DefaultLanguageHighlighterColors.INSTANCE_FIELD)
    private val function =
        TextAttributesKey.createTextAttributesKey("COBOL_FUNCTIONS", DefaultLanguageHighlighterColors.STATIC_METHOD)
    private val string =
        TextAttributesKey.createTextAttributesKey("COBOL_STRING", DefaultLanguageHighlighterColors.STRING)
    private val keyword =
        TextAttributesKey.createTextAttributesKey("COBOL_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD)
    private val dot = TextAttributesKey.createTextAttributesKey("COBOL_DOT", DefaultLanguageHighlighterColors.DOT)
    private val number =
        TextAttributesKey.createTextAttributesKey("COBOL_NUMBER", DefaultLanguageHighlighterColors.NUMBER)

    override fun getHighlightingLexer() = CobolLexerAdapter()

    override fun getTokenHighlights(tokenType: IElementType?) = when (tokenType) {
        CobolTypes.COMMENT -> comment
        CobolTypes.VARNAME -> vars
        CobolTypes.DISPLAY, CobolTypes.MOVE, CobolTypes.PERFORM, CobolTypes.CALL, CobolTypes.ADD, CobolTypes.WRITE, CobolTypes.READ -> function
        CobolTypes.STRING -> string
        CobolTypes.NUMBER, CobolTypes.ZERO, CobolTypes.SPACE, CobolTypes.LOW_VALUE, CobolTypes.HIGH_VALUE -> number

        CobolTypes.DIVISION, CobolTypes.SECTION,
        CobolTypes.IDENTIFICATION, CobolTypes.PROGRAM_ID, CobolTypes.AUTHOR, CobolTypes.DATE_WRITTEN, CobolTypes.INSTALLATION,
        CobolTypes.ENVIRONMENT,
        CobolTypes.CONFIGURATION, CobolTypes.SPECIAL_NAMES, CobolTypes.IS,
        CobolTypes.INPUT_OUTPUT, CobolTypes.FILE_CONTROL, CobolTypes.SELECT, CobolTypes.ASSIGN, CobolTypes.FILE, CobolTypes.STATUS,
        CobolTypes.TO, CobolTypes.ON, CobolTypes.DEPENDING, CobolTypes.OCCURS,

        CobolTypes.IF, CobolTypes.THEN, CobolTypes.ELSE, CobolTypes.END_IF,

        CobolTypes.WORKING_STORAGE,
        CobolTypes.FILE, CobolTypes.FD, CobolTypes.RECORD, CobolTypes.RECORDING, CobolTypes.STANDARD, CobolTypes.LABEL,

        CobolTypes.PIC, CobolTypes.X, CobolTypes.A, CobolTypes.S9, CobolTypes.VALUE,

        CobolTypes.USING,

        CobolTypes.EXEC, CobolTypes.END_EXEC, CobolTypes.SQL,

        CobolTypes.DATA,
        CobolTypes.PROCEDURE -> keyword

        TokenType.BAD_CHARACTER -> badCharacter
        CobolTypes.DOT -> dot

        TokenType.WHITE_SPACE, CobolTypes.ANY -> null
        else -> null
    }.let { arrayOf(it) }
}
