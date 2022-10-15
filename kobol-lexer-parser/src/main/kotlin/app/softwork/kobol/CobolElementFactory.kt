package app.softwork.kobol

import com.intellij.openapi.project.*
import com.intellij.psi.*

object CobolElementFactory {
    fun createVarName(project: Project, name: String) = createProgram(project, name).idDiv.programIDClause.programIDID.varName

    private fun createProgram(project: Project, text: String): CobolProgram {
        val name = "dummy.cbl"
        val cobolText = """123456 IDENTIFICATION DIVISION. PROGRAM-ID. $text. PROCEDURE DIVISION. CONTINUE."""
        val file = PsiFileFactory.getInstance(project).createFileFromText(name, CobolFileType(), cobolText) as CobolFile
        for (child in file.children) {
            if (child is CobolProgram) {
                return child
            }
        }
        error("No CobolProgram found by using createFile")
    }
}
