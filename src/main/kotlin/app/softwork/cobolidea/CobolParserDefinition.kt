package app.softwork.cobolidea

import com.intellij.lang.*
import com.intellij.openapi.project.*
import com.intellij.psi.*
import com.intellij.psi.tree.*

object CobolParserDefinition : ParserDefinition {
    val file = IFileElementType(CobolLanguage)
    private val comments = TokenSet.create(CobolTypes.COMMENT)

    override fun createLexer(project: Project?) = CobolLexerAdapter
    override fun createParser(project: Project?) = CobolParser()
    override fun getFileNodeType() = file

    override fun getCommentTokens(): TokenSet = comments

    override fun getStringLiteralElements(): TokenSet = TokenSet.EMPTY

    override fun createElement(node: ASTNode?): PsiElement = CobolTypes.Factory.createElement(node)

    override fun createFile(viewProvider: FileViewProvider): PsiFile = CobolFile(viewProvider)
}
