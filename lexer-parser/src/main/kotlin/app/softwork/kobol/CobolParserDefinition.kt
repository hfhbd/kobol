package app.softwork.kobol

import com.intellij.lang.*
import com.intellij.lexer.*
import com.intellij.openapi.project.*
import com.intellij.psi.*
import com.intellij.psi.tree.*

public class CobolParserDefinition : ParserDefinition {
    public companion object {
        @JvmStatic
        public val FILE: IFileElementType = IFileElementType(CobolLanguage)
    }

    private val stringLiterals = TokenSet.create(CobolTypes.STRING)

    override fun createLexer(project: Project?): FlexAdapter = CobolLexerAdapter()
    override fun createParser(project: Project?): CobolParser = CobolParser()
    override fun getFileNodeType(): IFileElementType = FILE

    override fun getCommentTokens(): TokenSet = TokenSet.EMPTY

    override fun getStringLiteralElements(): TokenSet = stringLiterals

    override fun createElement(node: ASTNode?): PsiElement = CobolTypes.Factory.createElement(node)

    override fun createFile(viewProvider: FileViewProvider): CobolFile = CobolFile(viewProvider)
}
