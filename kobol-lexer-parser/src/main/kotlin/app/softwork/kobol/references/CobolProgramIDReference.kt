package app.softwork.kobol.references

import app.softwork.kobol.*
import com.intellij.codeInsight.lookup.*
import com.intellij.openapi.roots.*
import com.intellij.openapi.util.*
import com.intellij.psi.*

class CobolProgramIDReference(psiElement: CobolCallingNameProgramID, range: TextRange) :
    PsiReferenceBase<CobolCallingNameProgramID>(psiElement, range) {
    override fun resolve(): PsiElement? {
        val index = ProjectRootManager.getInstance(myElement.manager.project).fileIndex

        var found: CobolProgramIDID? = null
        index.iterateContent {
            val file = myElement.manager.findFile(it) as? CobolFile ?: return@iterateContent true
            val id = file.programOrNull?.idDiv ?: return@iterateContent true
            val myText = myElement.text.noIdea.drop(1).dropLast(1)
            val programID = id.programIDClause.programIDID
            if (programID.varName.text == myText) {
                found = programID
                return@iterateContent false
            }
            true
        }
        return found
    }

    override fun getVariants(): Array<LookupElement> {
        val index = ProjectRootManager.getInstance(myElement.manager.project).fileIndex

        val programs = buildList {
            index.iterateContent {
                val file = myElement.manager.findFile(it) as? CobolFile ?: return@iterateContent true
                val id = file.programOrNull?.idDiv ?: return@iterateContent true
                val myText = myElement.text.noIdea.drop(1).dropLast(1)
                val programID = id.programIDClause.programIDID
                val myProgram = programID.varName.text
                if (myProgram.startsWith(myText)) {
                    add(LookupElementBuilder.create(myProgram))
                    return@iterateContent false
                }
                true
            }
        }
        return programs.toTypedArray()
    }
}
