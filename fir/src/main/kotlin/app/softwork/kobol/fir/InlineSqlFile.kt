package app.softwork.kobol.fir

import com.alecstrong.sql.psi.core.*
import com.intellij.icons.*
import com.intellij.lang.Language
import com.intellij.openapi.fileTypes.*
import com.intellij.psi.*

internal class InlineSqlFile(viewProvider: FileViewProvider): SqlFileBase(viewProvider, SqlInlineLanguage) {
    override val order: Int? = null
    override fun getFileType() = InlineSqlFileType
}

internal object SqlInlineLanguage: Language("SqlInlineLanguage")

internal object InlineSqlFileType : LanguageFileType(SqlInlineLanguage) {
    override fun getIcon() = AllIcons.Providers.DB2
    override fun getName() = "Inline SqlFile"
    override fun getDefaultExtension() = "inlinesql"
    override fun getDescription() = "Inline SqlFile"
}
