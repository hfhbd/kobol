package app.softwork.kobol

import com.intellij.lexer.*
import com.intellij.psi.tree.*

public fun CobolLexerAdapter(): FlexAdapter {
    val lexer = CobolLexer(null)
    return FlexAdapter(object : FlexLexer by lexer {

        override fun advance(): IElementType? {
            val type: IElementType? = lexer.advance()
            return if (type == null) {
                lexer.yycolumn = 0
                null
            } else {
                type
            }
        }

        override fun reset(buf: CharSequence?, start: Int, end: Int, initialState: Int) {
            lexer.reset(buf, start, end, initialState)
            lexer.yycolumn = 0
        }
    })
}
