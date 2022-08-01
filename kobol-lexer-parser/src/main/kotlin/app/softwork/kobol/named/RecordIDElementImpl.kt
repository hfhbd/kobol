package app.softwork.kobol.named

import app.softwork.kobol.*
import app.softwork.kobol.references.*
import com.intellij.extapi.psi.*
import com.intellij.lang.*
import com.intellij.openapi.util.*
import com.intellij.psi.*

abstract class RecordIDElementImpl(node: ASTNode) : ASTWrapperPsiElement(node), CobolNamedElement, CobolRecordID {
    override fun getNameIdentifier(): PsiElement = varName
    override fun getName(): String = varName.text

    override fun setName(name: String): PsiElement {
        val new = CobolElementFactory.createVarName(project, name)
        node.replaceChild(varName.node, new.node)
        return this
    }

    override fun getReference(): CobolRecordReference {
        val range = varName.textRangeInParent
        return CobolRecordReference(this, range)
    }
}
