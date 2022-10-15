package app.softwork.kobol

import com.alecstrong.sql.psi.core.*
import com.intellij.icons.*
import com.intellij.lang.Language
import com.intellij.openapi.fileTypes.*
import com.intellij.psi.*

class InlineSqlFile(viewProvider: FileViewProvider): SqlFileBase(viewProvider, SqlInlineLanguage) {
    override val order: Int? = null
    override fun getFileType() = InlineSqlFileType
}

object SqlInlineLanguage: Language("SqlInlineLanguage")

object InlineSqlFileType : LanguageFileType(SqlInlineLanguage) {
    override fun getIcon() = AllIcons.Providers.DB2
    override fun getName() = "Inline SqlFile"
    override fun getDefaultExtension() = "inlinesql"
    override fun getDescription() = "Inline SqlFile"
}
