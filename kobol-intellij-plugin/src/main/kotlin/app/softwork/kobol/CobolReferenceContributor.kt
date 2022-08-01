package app.softwork.kobol

import app.softwork.kobol.references.*
import com.intellij.openapi.util.*
import com.intellij.patterns.PlatformPatterns.*
import com.intellij.psi.*
import com.intellij.util.*

class CobolReferenceContributor : PsiReferenceContributor() {
    override fun registerReferenceProviders(registrar: PsiReferenceRegistrar) {
        registrar.registerReferenceProvider(
            psiElement(CobolTypes.VARNAME).withParent(CobolRecordID::class.java),
            object : PsiReferenceProvider() {
                override fun getReferencesByElement(
                    element: PsiElement,
                    context: ProcessingContext
                ): Array<PsiReference> {
                    val parent = element.parent as CobolRecordID
                    return arrayOf(CobolRecordReference(parent, TextRange.from(0, element.textLength)))
                }
            }
        )
        registrar.registerReferenceProvider(
            psiElement(CobolTypes.VARNAME).withParent(psiElement(CobolVariable::class.java)),
            object : PsiReferenceProvider() {
                override fun getReferencesByElement(
                    element: PsiElement,
                    context: ProcessingContext
                ): Array<PsiReference> {
                    val parent = element.parent as CobolVariable
                    return arrayOf(CobolVariableReference(parent, TextRange.from(0, element.textLength)))
                }
            }
        )
        registrar.registerReferenceProvider(
            psiElement(CobolTypes.VARNAME).withParent(CobolSectionID::class.java),
            object : PsiReferenceProvider() {
                override fun getReferencesByElement(
                    element: PsiElement,
                    context: ProcessingContext
                ): Array<PsiReference> {
                    val parent = element.parent as CobolSectionID
                    return arrayOf(CobolSectionReference(parent, TextRange.from(0, element.textLength)))
                }
            }
        )
    }
}
