package app.softwork.kobol

import app.softwork.kobol.CobolTypes.*
import com.intellij.openapi.editor.*
import com.intellij.openapi.editor.colors.TextAttributesKey.*
import com.intellij.openapi.fileTypes.*
import com.intellij.psi.*
import com.intellij.psi.tree.*
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors as Default

object CobolSyntaxHighlighter : SyntaxHighlighterBase() {
    private val comment = createTextAttributesKey("COBOL_COMMENT", Default.LINE_COMMENT)
    private val badCharacter = createTextAttributesKey("COBOL_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER)
    private val vars = createTextAttributesKey("COBOL_VAR", Default.INSTANCE_FIELD)
    private val function = createTextAttributesKey("COBOL_FUNCTIONS", Default.STATIC_METHOD)
    private val string = createTextAttributesKey("COBOL_STRING", Default.STRING)
    private val keyword = createTextAttributesKey("COBOL_KEYWORD", Default.KEYWORD)
    private val dot = createTextAttributesKey("COBOL_DOT", Default.DOT)
    private val number = createTextAttributesKey("COBOL_NUMBER", Default.NUMBER)

    override fun getHighlightingLexer() = CobolLexerAdapter

    override fun getTokenHighlights(tokenType: IElementType?) = when (tokenType) {
        COMMENT -> comment
        VARNAME -> vars
        DISPLAY, MOVE, PERFORM, CALL, ADD -> function
        STRING -> string
        NUMBER, ZERO, SPACE, LOW_VALUE, HIGH_VALUE -> number

        DIVISION, SECTION,
        IDENTIFICATION, PROGRAM_ID, AUTHOR, DATE_WRITTEN, INSTALLATION,
        ENVIRONMENT,
        CONFIGURATION, SPECIAL_NAMES, IS,
        INPUT_OUTPUT, FILE_CONTROL, SELECT, ASSIGN, FILE, STATUS,
        TO, ON, DEPENDING, OCCURS,

        WORKING_STORAGE,
        FILE, FD, RECORD, RECORDING, STANDARD, LABEL,

        PIC, X, A, S9, VALUE,

        USING,

        DATA,
        PROCEDURE -> keyword

        TokenType.BAD_CHARACTER -> badCharacter
        DOT -> dot

        TokenType.WHITE_SPACE, ANY -> null
        else -> null
    }.let { arrayOf(it) }
}
