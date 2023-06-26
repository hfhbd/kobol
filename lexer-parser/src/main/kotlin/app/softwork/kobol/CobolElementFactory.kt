package app.softwork.kobol

import com.intellij.openapi.project.*
import com.intellij.openapi.roots.*
import com.intellij.psi.*

public object CobolElementFactory {
    internal fun createVarName(project: Project, name: String): PsiElement =
        createProgram(project, name).idDiv.programIDClause.programIDID.varName

    private fun createProgram(project: Project, text: String): CobolProgram {
        val name = "dummy.cbl"
        val cobolText = """123456 IDENTIFICATION DIVISION. PROGRAM-ID. $text. PROCEDURE DIVISION. CONTINUE."""
        val file =
            PsiFileFactory.getInstance(project).createFileFromText(name, CobolFileType.INSTANCE, cobolText) as CobolFile
        return file.program
    }
    
    private const val sqlca = """
            01 SQLCA SYNC.
                05 SQLCAID PIC X(8) VALUE "SQLCA   ".
                05 SQLCABC PIC S9(9) COMP-5 VALUE 136.
                05 SQLCODE PIC S9(9) COMP-5.
                05 SQLERRM.
                05 SQLERRP PIC X(8).
                05 SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5.
                05 SQLWARN.
                    10 SQLWARN0 PIC X.
                    10 SQLWARN1 PIC X.
                    10 SQLWARN2 PIC X.
                    10 SQLWARN3 PIC X.
                    10 SQLWARN4 PIC X.
                    10 SQLWARN5 PIC X.
                    10 SQLWARN6 PIC X.
                    10 SQLWARN7 PIC X.
                    10 SQLWARN8 PIC X.
                    10 SQLWARN9 PIC X.
                    10 SQLWARNA PIC X.
                05 SQLSTATE PIC X(5).
        """

    public fun includeSQL(project: Project, fileName: String): List<CobolRecordDef> {
        val name = "dummy.cbl"
        var text: String? = null
        val fileIndex = project.getService(ProjectFileIndex::class.java)
        fileIndex.iterateContent { file ->
            if (file.nameWithoutExtension == fileName) {
                text = String(file.contentsToByteArray())
                false
            } else true
        }
        if (text == null && fileName == "SQLCA") {
            text = sqlca
        }
        val fileText = requireNotNull(text) { "File $fileName not found in $fileIndex" }
        val addLineNumber = fileText.lines().map { it.trim() }.mapNotNull {
            if (it.isEmpty()) {
                null
            } else if (it.substring(0..5).toIntOrNull() != null && (it[6] in setOf('*', ' ', '/'))) {
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
        return cobolFile.program.dataDiv!!.workingStorageSection!!.stmList.map { it.recordDef!! }
    }
}
