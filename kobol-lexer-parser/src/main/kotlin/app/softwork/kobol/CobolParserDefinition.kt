package app.softwork.kobol

import com.intellij.lang.*
import com.intellij.openapi.project.*
import com.intellij.psi.*
import com.intellij.psi.tree.*

class CobolParserDefinition : ParserDefinition {
    companion object {
        val FILE = IFileElementType(CobolLanguage)
    }

    private val stringLiterals = TokenSet.create(CobolTypes.STRING)

    override fun createLexer(project: Project?) = CobolLexerAdapter()
    override fun createParser(project: Project?) = CobolParser()
    override fun getFileNodeType() = FILE

    override fun getCommentTokens(): TokenSet = TokenSet.EMPTY

    override fun getStringLiteralElements() = stringLiterals

    override fun createElement(node: ASTNode?): PsiElement = CobolTypes.Factory.createElement(node)

    override fun createFile(viewProvider: FileViewProvider) = CobolFile(viewProvider)
}
