package app.softwork.kobol.named

import app.softwork.kobol.*
import app.softwork.kobol.references.*
import com.intellij.extapi.psi.*
import com.intellij.lang.*
import com.intellij.openapi.util.*
import com.intellij.psi.*

abstract class CobolVariableElementImpl(node: ASTNode) : ASTWrapperPsiElement(node), CobolNamedElement, CobolVariable {
    override fun getNameIdentifier(): PsiElement = varName
    override fun getName(): String = varName.text

    override fun setName(name: String): PsiElement {
        val new = CobolElementFactory.createVarName(project, name)
        node.replaceChild(varName.node, new.node)
        return this
    }

    override fun getReference(): CobolVariableReference {
        return CobolVariableReference(this, textRangeInParent)
    }

    override fun getReferences(): Array<PsiReference> {
        val varName = CobolVariableReference(this, TextRange.from(0, varName.textLength))
        val of = ofClause
        if (of != null) {
            val size = textLength - 1
            val varLength = of.recordID.varName.textLength
            //BAR OF B
            //01234567
            val range = TextRange.create(size - varLength, size)
            val record = object: PsiReference by CobolRecordReference(of.recordID, range) {
                override fun getElement(): PsiElement {
                    return this@CobolVariableElementImpl
                }
            }
            return arrayOf(varName, record)
        }
        return arrayOf(varName)
    }
}
