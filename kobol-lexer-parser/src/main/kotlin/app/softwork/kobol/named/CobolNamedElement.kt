package app.softwork.kobol.named

import com.intellij.psi.PsiNameIdentifierOwner

interface CobolNamedElement: PsiNameIdentifierOwner {
    override fun getName(): String
}
