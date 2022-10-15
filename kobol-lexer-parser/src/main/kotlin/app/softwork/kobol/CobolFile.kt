package app.softwork.kobol

import com.intellij.extapi.psi.*
import com.intellij.psi.*

class CobolFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, CobolLanguage) {
    override fun getFileType() = CobolFileType()
    override fun toString() = "Cobol File"
}
