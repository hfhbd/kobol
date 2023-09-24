package app.softwork.kobol.references

import app.softwork.kobol.*
import com.intellij.codeInsight.lookup.*
import com.intellij.openapi.util.*
import com.intellij.psi.*

public class CobolVariableReference(psiElement: CobolVariable, range: TextRange) :
    PsiReferenceBase<CobolVariable>(psiElement, range), PsiPolyVariantReference {
    override fun resolve(): CobolRecordDef? {
        return multiResolve(false).singleOrNull()?.element as CobolRecordDef?
    }

    override fun multiResolve(incompleteCode: Boolean): Array<ResolveResult> = find(incompleteCode) {
        PsiElementResolveResult(it)
    }.toTypedArray()

    override fun getVariants(): Array<LookupElement> = find(incompleteCode = true) {
        LookupElementBuilder.create(it.recordID!!.varName.text).withIcon(CobolFileType.icon)
            .withTypeText("VARIANT TEST")
    }.toTypedArray()

    private fun <T> find(incompleteCode: Boolean = false, action: (CobolRecordDef) -> T): List<T> {
        val records = records(myElement) ?: return emptyList()
        val myName = myElement.varName.text.noIdea
        val ofName = myElement.ofClause?.recordID?.varName?.text?.noIdea
        return buildList {
            var currentRecord: CobolRecordDef? = null
            for (recordDef in records) {
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

                if (incompleteCode) {
                    add(action(recordDef))
                } else if (currentRecord != null && ofName != null) {
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
