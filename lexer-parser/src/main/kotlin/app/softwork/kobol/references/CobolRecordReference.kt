package app.softwork.kobol.references

import app.softwork.kobol.*
import com.intellij.codeInsight.lookup.*
import com.intellij.openapi.util.*
import com.intellij.psi.*

public class CobolRecordReference(psiElement: CobolRecordID, range: TextRange) :
    PsiReferenceBase<CobolRecordID>(psiElement, range) {
    override fun resolve(): CobolRecordDef? {
        val file = myElement.containingFile as CobolFile
        val stm: List<CobolStm> =
            file.programOrNull?.dataDiv?.workingStorageSection?.stmList ?: return null
        val myName = myElement.varName.text.noIdea
        for (stmt in stm) {
            val record = stmt.recordDef ?: continue
            val recordName = record.recordID?.varName?.text ?: continue
            val recordNumber = record.number.text.toInt()
            if (recordNumber == 1 && recordName == myName) {
                return record
            }
        }
        return null
    }

    override fun getVariants(): Array<LookupElement> {
        val file = myElement.containingFile as CobolFile
        val stm: List<CobolStm> =
            file.programOrNull?.dataDiv?.workingStorageSection?.stmList ?: return emptyArray()
        val myName = myElement.varName.text.noIdea
        return buildList {
            for (stmt in stm) {
                val record = stmt.recordDef ?: continue
                val recordName = record.recordID?.varName?.text ?: continue
                val recordNumber = record.number.text.toInt()
                if (recordNumber == 1 && recordName.startsWith(myName)) {
                    add(
                        LookupElementBuilder.create(recordName).withIcon(CobolFileType.icon)
                            .withTypeText("VARIANT TEST")
                    )
                }
            }
        }.toTypedArray()
    }
}
