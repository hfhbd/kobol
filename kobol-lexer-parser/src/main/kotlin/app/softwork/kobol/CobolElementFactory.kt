package app.softwork.kobol

import com.intellij.openapi.project.*
import com.intellij.openapi.roots.*
import com.intellij.psi.*

object CobolElementFactory {
    fun createVarName(project: Project, name: String) =
        createProgram(project, name).idDiv.programIDClause.programIDID.varName

    private fun createProgram(project: Project, text: String): CobolProgram {
        val name = "dummy.cbl"
        val cobolText = """123456 IDENTIFICATION DIVISION. PROGRAM-ID. $text. PROCEDURE DIVISION. CONTINUE."""
        val file = PsiFileFactory.getInstance(project).createFileFromText(name, CobolFileType.INSTANCE, cobolText) as CobolFile
        for (child in file.children) {
            if (child is CobolProgram) {
                return child
            }
        }
        error("No CobolProgram found by using createFile")
    }

    fun includeSQL(project: Project, fileName: String): List<CobolRecordDef> {
        val name = "dummy.cbl"
        var text: String? = null
        val fileIndex = project.getService(ProjectFileIndex::class.java)
        fileIndex.iterateContent { file ->
            if (file.nameWithoutExtension == fileName) {
                text = String(file.contentsToByteArray())
                false
            } else true
        }
        val fileText = requireNotNull(text)
        val addLineNumber = fileText.lines().mapNotNull {
            if (it.substring(0..5).toIntOrNull() != null && (it[6] in setOf('*', ' ', '/'))) {
                it
            } else {
                val first = it.firstOrNull() ?: return@mapNotNull null
                if (first == '*' || first == '/') {
                    "123456$it"
                } else "123456 $it"
            }
        }.joinToString("\n", prefix = "\n", postfix = "\n123456")

        val cobolText =
            """123456 IDENTIFICATION DIVISION. PROGRAM-ID. INCLUDESQL. DATA DIVISION. WORKING-STORAGE SECTION. $addLineNumber PROCEDURE DIVISION. CONTINUE."""
        val cobolFile =
            PsiFileFactory.getInstance(project).createFileFromText(name, CobolFileType.INSTANCE, cobolText) as CobolFile
        for (child in cobolFile.children) {
            if (child is CobolProgram) {
                return child.dataDiv!!.workingStorageSection!!.stmList.map { it.recordDef!! }
            }
        }
        error("No CobolProgram found by using createFile")
    }
}
