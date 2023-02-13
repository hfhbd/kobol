package app.softwork.kobol.intellij

import app.softwork.kobol.*
import com.intellij.lang.documentation.*
import com.intellij.psi.*

internal class CobolDocumentationProvider : AbstractDocumentationProvider() {
    override fun generateDoc(element: PsiElement, originalElement: PsiElement?): String? {
        val comments = if (element is CobolRecordDef) {
            element.comments
        } else if (element is CobolProcedureSection) {
            element.comments
        } else null
        return comments?.asComments()?.takeIf { it.isNotEmpty() }?.joinToString(separator = "<br>")
    }
}
