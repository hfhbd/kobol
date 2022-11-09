package app.softwork.kobol

import com.intellij.extapi.psi.*
import com.intellij.psi.*
import com.intellij.psi.util.*

class CobolFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, CobolLanguage) {
    override fun getFileType() = CobolFileType.INSTANCE
    override fun toString() = "Cobol File"

    val program: CobolProgram get() = programOrNull ?: error(childrenOfType<PsiErrorElement>().joinToString {
        it.errorDescription
    })

    val programOrNull: CobolProgram? get() = childrenOfType<CobolProgram>().singleOrNull()

}
