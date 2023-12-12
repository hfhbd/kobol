package app.softwork.kobol

import com.intellij.extapi.psi.*
import com.intellij.psi.*
import com.intellij.psi.util.*
import java.nio.file.Path
import kotlin.io.path.name

public class CobolFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, CobolLanguage) {
    override fun getFileType(): CobolFileType = CobolFileType.INSTANCE
    override fun toString(): String = "Cobol File"

    public val program: CobolProgram get() = programOrNull ?: error(
        childrenOfType<PsiErrorElement>().joinToString {
            it.errorDescription
        },
    )

    public val programOrNull: CobolProgram? get() = childrenOfType<CobolProgram>().singleOrNull()
}
