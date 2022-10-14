package app.softwork.kobol

import com.intellij.openapi.fileTypes.*
import com.intellij.openapi.util.*
import javax.swing.*

class CobolFileType : LanguageFileType(CobolLanguage) {
    override fun getName() = "Cobol File"
    override fun getDescription() = "Cobol File"

    override fun getDefaultExtension() = Companion.defaultExtension

    override fun getIcon(): Icon = Companion.icon

    companion object {
        const val defaultExtension = "cbl"
        val icon = IconLoader.getIcon("/icons/icon.png", CobolFileType::class.java)
    }
}
