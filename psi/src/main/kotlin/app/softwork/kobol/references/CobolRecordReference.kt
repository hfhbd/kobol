package app.softwork.kobol.references

import app.softwork.kobol.*
import com.intellij.codeInsight.lookup.*
import com.intellij.openapi.util.*
import com.intellij.psi.*

public class CobolRecordReference(psiElement: CobolRecordID, range: TextRange) :
    PsiReferenceBase<CobolRecordID>(psiElement, range) {
    override fun resolve(): CobolRecordDef? {
        val records = allRecords(myElement) ?: return null
        val myName = myElement.name
        val of = (myElement.parent as? CobolVariable)?.ofClause?.recordID?.name
        if (myName == of) { return null }
        for (record in records) {
            val recordName = record.recordID?.varName?.text ?: continue
            if (recordName == myName) {
                return record
            }
        }
        return null
    }

    override fun getVariants(): Array<LookupElement> {
        val records = allRecords(myElement) ?: return emptyArray()
        val myName = myElement.varName.text.noIdea
        return buildList {
            for (record in records) {
                val recordName = record.recordID?.varName?.text ?: continue
                val recordNumber = record.number.text.toInt()
                if (recordNumber == 1 && recordName.startsWith(myName)) {
                    add(
                        LookupElementBuilder.create(recordName).withIcon(CobolFileType.icon)
                            .withTypeText("VARIANT TEST"),
                    )
                }
            }
        }.toTypedArray()
    }

    private fun allRecords(myElement: PsiElement): List<CobolRecordDef>? {
        val file = myElement.containingFile as CobolFile
        val dataDiv = file.programOrNull?.dataDiv ?: return null
        val workingStmts: List<CobolRecordDef> =
            dataDiv.workingStorageSection?.stmList?.mapNotNull {
                it.recordDef
            } ?: emptyList()
        val linkingStmts: List<CobolRecordDef> = dataDiv.linkingSection?.recordDefList ?: emptyList()
        if (workingStmts.isEmpty() && linkingStmts.isEmpty()) {
            return null
        }
        return workingStmts + linkingStmts
    }
}

internal val String.noIdea get() = removeSuffix("IntellijIdeaRulezzz")
