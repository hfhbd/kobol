package app.softwork.kobol.intellij

import app.softwork.kobol.CobolProcedureDiv
import app.softwork.kobol.CobolRecordID
import app.softwork.kobol.CobolVariable
import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.lang.annotation.Annotator
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.psi.PsiElement
import com.intellij.psi.util.elementType
import com.intellij.psi.util.parentOfType

internal class CobolAnnotator : Annotator {
    override fun annotate(element: PsiElement, holder: AnnotationHolder) {
        if (element is CobolRecordID && element.parentOfType<CobolProcedureDiv>() != null) {
            val name = element.name
            val of = (element.parent as? CobolVariable)?.ofClause?.recordID?.name
            if (name == of) {
                return holder.newAnnotation(HighlightSeverity.ERROR, "$name not unique")
                    .highlightType(ProblemHighlightType.ERROR)
                    .range(element)
                    .create()
            }
            if (element.reference?.resolve() == null) {
                holder.newAnnotation(HighlightSeverity.ERROR, "$name not found")
                    .highlightType(ProblemHighlightType.LIKE_UNKNOWN_SYMBOL)
                    .range(element)
                    .create()
            }
        }
    }
}
