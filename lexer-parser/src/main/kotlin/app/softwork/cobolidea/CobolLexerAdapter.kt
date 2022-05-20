package app.softwork.cobolidea

import com.intellij.lexer.*

object CobolLexerAdapter : FlexAdapter(CobolLexer(null))
