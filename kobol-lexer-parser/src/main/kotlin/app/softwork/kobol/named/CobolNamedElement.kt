package app.softwork.kobol.named

import com.intellij.psi.PsiNameIdentifierOwner

internal interface CobolNamedElement: PsiNameIdentifierOwner {
    override fun getName(): String
}
