package app.softwork.kobol.references

import app.softwork.kobol.*
import com.intellij.codeInsight.lookup.*
import com.intellij.openapi.util.*
import com.intellij.psi.*

class CobolSectionReference(psiElement: CobolSectionID, range: TextRange) :
    PsiReferenceBase<CobolSectionID>(psiElement, range) {
    override fun resolve(): PsiElement? {
        val file = myElement.containingFile as CobolFile
        val procedures = file.findChildByClass(CobolProgram::class.java)?.procedureDiv ?: return null
        val section = procedures.procedureSectionList.find {
            it.sectionID.varName.text == myElement.varName.text
        }
        return section
    }

    override fun getVariants(): Array<LookupElement> {
        val file = myElement.containingFile as CobolFile
        val procedures = file.findChildByClass(CobolProgram::class.java)?.procedureDiv ?: return emptyArray()
        val sections = buildList {
            for (section in procedures.procedureSectionList) {
                val sectionName = section.sectionID.varName.text
                val myText = element.varName.text.removeSuffix("IntellijIdeaRulezzz")
                if (sectionName.startsWith(myText)) {
                    add(
                        LookupElementBuilder.create(sectionName)
                            .withIcon(CobolFileType.icon)
                            .withTypeText("TEST")
                            .withTailText(sectionName, true)
                    )
                }
            }
        }
        return sections.toTypedArray()
    }
}
