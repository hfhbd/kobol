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

    // https://www.ibm.com/docs/en/db2-for-zos/11?topic=sqlca-included#d449577e134
    // TODO: 05 SQLERRM between SQLCODE and SQLERRP.
    // TODO: 05 SQLWARN before SQLWARN0.
    // TODO: 05 SQLEXT between 7 and 8.
    private const val sqlca = """
            01 SQLCA.
               05 SQLCAID     PIC X(8) VALUE "SQLCA   ".
               05 SQLCABC     PIC S9(9) COMP-5 VALUE 136.
               05 SQLCODE     PIC S9(9) COMP-5.
               05 SQLERRML    PIC S9(4) COMP-5.
               05 SQLERRMC    PIC X(70).
               05 SQLERRP     PIC X(8).
               05 SQLERRD     OCCURS 6 TIMES
                              PIC S9(9) COMP-5.
               
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
            } else {
                true
            }
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
                } else {
                    "123456 $it"
                }
            }
        }.joinToString("\n", prefix = "\n", postfix = "\n123456")

        // language=cobol
        val cobolText =
            """
               |123456 IDENTIFICATION DIVISION. 
               |123456 PROGRAM-ID. INCLUDESQL. 
               |123456 DATA DIVISION. 
               |123456 WORKING-STORAGE SECTION. 
               |$addLineNumber
               |123456 PROCEDURE DIVISION. 
               |123456 CONTINUE.
            """.trimMargin()
        val cobolFile =
            PsiFileFactory.getInstance(project).createFileFromText(name, CobolFileType.INSTANCE, cobolText) as CobolFile
        return cobolFile.program.dataDiv!!.workingStorageSection!!.stmList.map { it.recordDef!! }
    }
}
