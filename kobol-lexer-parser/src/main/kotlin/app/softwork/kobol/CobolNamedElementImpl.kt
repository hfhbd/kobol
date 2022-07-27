package app.softwork.kobol

import com.intellij.extapi.psi.*
import com.intellij.lang.*
import com.intellij.psi.*

abstract class CobolNamedElementImpl(node: ASTNode) : ASTWrapperPsiElement(node), CobolNamedElement, CobolId {
    override fun getNameIdentifier(): PsiElement = varName
    override fun getName(): String = varName.text

    override fun setName(name: String): PsiElement {
        val new = CobolElementFactory.createVarName(project, name)
        node.replaceChild(varName.node, new.node)
        return this
    }
}
