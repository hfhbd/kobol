package app.softwork.kobol

import app.softwork.sqldelight.db2dialect.Db2Dialect
import com.alecstrong.sql.psi.core.*
import com.intellij.icons.*
import com.intellij.lang.*
import com.intellij.openapi.fileTypes.*
import com.intellij.openapi.util.*
import com.intellij.psi.*

internal class InlineSqlFile(viewProvider: FileViewProvider) : SqlFileBase(viewProvider, SqlInlineLanguage) {
    override val order: Long? = null
    override fun getFileType() = InlineSqlFileType

    override fun baseContributorFiles(): List<SqlFileBase> {
        val base = super.baseContributorFiles()
        if (getUserData(isPredefined) == Unit) {
            return base
        }
        val factory = PsiFileFactory.getInstance(project)
        return base + Db2Dialect.predefinedSystemSchema.map {
            val file = factory.createFileFromText(SqlInlineLanguage, it) as SqlFileBase
            file.putUserData(isPredefined, Unit)
            file
        }
    }

    companion object {
        val isPredefined = Key.create<Unit>("isPredefined")
    }
}

internal val SqlInlineLanguage = object : Language("SqlInlineLanguage") {}

internal object InlineSqlFileType : LanguageFileType(SqlInlineLanguage) {
    override fun getIcon() = AllIcons.Providers.DB2
    override fun getName() = "Inline SqlFile"
    override fun getDefaultExtension() = "inlinesql"
    override fun getDescription() = "Inline SqlFile"
}
