package app.softwork.kobol

import com.intellij.openapi.fileTypes.*
import com.intellij.openapi.util.*
import javax.swing.*

object CobolFileType : LanguageFileType(CobolLanguage) {
    override fun getName() = "Cobol File"
    override fun getDescription() = "Cobol File"

    override fun getDefaultExtension() = "cbl"

    override fun getIcon(): Icon = fileIcon
    private val fileIcon = IconLoader.getIcon("/icons/icon.png", CobolFileType::class.java)
}
