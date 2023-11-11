package app.softwork.kobol.intellij

import app.softwork.kobol.*
import app.softwork.kobol.references.*
import com.intellij.openapi.util.*
import com.intellij.patterns.*
import com.intellij.psi.*
import com.intellij.util.*

internal class CobolReferenceContributor : PsiReferenceContributor() {
    override fun registerReferenceProviders(registrar: PsiReferenceRegistrar) {
        registrar.registerReferenceProvider(
            PlatformPatterns.psiElement(CobolTypes.VARNAME).withParent(CobolRecordID::class.java),
            object : PsiReferenceProvider() {
                override fun getReferencesByElement(
                    element: PsiElement,
                    context: ProcessingContext,
                ): Array<PsiReference> {
                    val parent = element.parent as CobolRecordID
                    return arrayOf(CobolRecordReference(parent, TextRange.from(0, element.textLength)))
                }
            },
        )

        registrar.registerReferenceProvider(
            PlatformPatterns.psiElement(CobolTypes.VARNAME).withParent(CobolSectionID::class.java),
            object : PsiReferenceProvider() {
                override fun getReferencesByElement(
                    element: PsiElement,
                    context: ProcessingContext,
                ): Array<PsiReference> {
                    val parent = element.parent as CobolSectionID
                    return arrayOf(CobolSectionReference(parent, TextRange.from(0, element.textLength)))
                }
            },
        )
        registrar.registerReferenceProvider(
            PlatformPatterns.psiElement(CobolTypes.STRING).withParent(CobolCallingNameProgramID::class.java),
            object : PsiReferenceProvider() {
                override fun getReferencesByElement(
                    element: PsiElement,
                    context: ProcessingContext,
                ): Array<PsiReference> {
                    val parent = element.parent as CobolCallingNameProgramID
                    return arrayOf(CobolProgramIDReference(parent, TextRange.from(0, element.textLength)))
                }
            },
        )
    }
}
