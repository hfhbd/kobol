package app.softwork.kobol

import com.intellij.lexer.*
import com.intellij.psi.tree.*

private val lexer = CobolLexer(null)

object CobolLexerAdapter : FlexAdapter(object : FlexLexer by lexer {
    override fun advance(): IElementType? {
        val type: IElementType? = lexer.advance()
        return if (type == null) {
            lexer.yycolumn = 0
            null
        } else type
    }

    override fun reset(buf: CharSequence?, start: Int, end: Int, initialState: Int) {
        lexer.reset(buf, start, end, initialState)
        lexer.yycolumn = 0
    }
})
