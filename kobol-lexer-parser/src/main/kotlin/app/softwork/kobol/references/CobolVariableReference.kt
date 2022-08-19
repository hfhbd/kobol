package app.softwork.kobol.references

import app.softwork.kobol.*
import com.intellij.codeInsight.lookup.*
import com.intellij.openapi.util.*
import com.intellij.psi.*

class CobolVariableReference(psiElement: CobolVariable, range: TextRange) :
    PsiReferenceBase<CobolVariable>(psiElement, range), PsiPolyVariantReference {
    override fun resolve(): PsiElement? {
        return multiResolve(false).singleOrNull()?.element
    }

    override fun multiResolve(incompleteCode: Boolean): Array<ResolveResult> = find(incompleteCode) {
        PsiElementResolveResult(it)
    }.toTypedArray()

    override fun getVariants(): Array<LookupElement> = find(incompleteCode = true) {
        LookupElementBuilder.create(it.recordID!!.varName.text).withIcon(CobolFileType.icon)
            .withTypeText("VARIANT TEST")
    }.toTypedArray()

    private fun <T> find(incompleteCode: Boolean = false, action: (CobolRecordDef) -> T): List<T> {
        val file = myElement.containingFile as CobolFile
        val stm: List<CobolStm> =
            file.findChildByClass(CobolProgram::class.java)?.dataDiv?.workingStorageSection?.stmList
                ?: return emptyList()
        val myName = myElement.varName.text.noIdea
        val ofName = myElement.ofClause?.recordID?.varName?.text?.noIdea
        return buildList {
            var currentRecord: CobolRecordDef? = null
            for (stmt in stm) {
                val recordDef = stmt.recordDef ?: continue
                val recordName = recordDef.recordID?.varName?.text ?: continue
                if (recordDef.number.text.toInt() == 1) {
                    currentRecord = recordDef
                    continue
                }
                fun action() {
                    if (incompleteCode && recordName.startsWith(myName)) {
                        add(action(recordDef))
                    } else if (recordDef.recordID?.varName?.text == myName) {
                        add(action(recordDef))
                    }
                }

                if (currentRecord != null && ofName != null) {
                    if (currentRecord.recordID?.varName?.text == ofName) {
                        action()
                    }
                } else {
                    action()
                }
            }
        }
    }
}

internal val String.noIdea get() = removeSuffix("IntellijIdeaRulezzz")
