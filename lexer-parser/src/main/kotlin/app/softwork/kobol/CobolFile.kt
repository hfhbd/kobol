package app.softwork.kobol

import com.intellij.extapi.psi.*
import com.intellij.psi.*
import com.intellij.psi.util.*

class CobolFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, CobolLanguage) {
    override fun getFileType() = CobolFileType

    override fun toString() = "Cobol File"

    fun getProgramName(): String {
        for (element in children) {
            if (element.elementType == CobolTypes.PROGRAM_ID) {
                for (possibleName in element.children) {
                    if (possibleName.elementType == CobolTypes.VARNAME) {
                        return possibleName.text
                    }
                }
            }
        }
        error("No PROGRAM-ID found")
    }

    fun procedure(): CobolDisplays {
        for(element in children) {
            if(element.elementType == CobolTypes.PROCEDURE) {
                return element.children[1] as CobolDisplays
            }
        }
        error("No PROCEDURE DIVISION found")
    }
}
