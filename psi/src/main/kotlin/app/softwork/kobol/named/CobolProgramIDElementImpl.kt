package app.softwork.kobol.named

import app.softwork.kobol.*
import app.softwork.kobol.references.*
import com.intellij.extapi.psi.*
import com.intellij.lang.*
import com.intellij.openapi.util.*
import com.intellij.psi.*
import com.intellij.psi.impl.source.tree.LeafElement

internal abstract class CobolProgramIDElementImpl(node: ASTNode) :
    ASTWrapperPsiElement(node),
    CobolNamedElement,
    CobolCallingNameProgramID {
    override fun getNameIdentifier(): PsiElement = this
    override fun getName(): String = text

    override fun setName(name: String): PsiElement {
        val node = node as LeafElement
        node.replaceWithText(name)
        return this
    }

    override fun getReference(): CobolProgramIDReference = CobolProgramIDReference(this, TextRange.from(0, textLength))
}
