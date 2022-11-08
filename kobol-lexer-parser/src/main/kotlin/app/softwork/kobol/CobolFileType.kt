package app.softwork.kobol

import com.intellij.openapi.fileTypes.*
import com.intellij.openapi.util.*
import javax.swing.*

class CobolFileType private constructor(): LanguageFileType(CobolLanguage) {
    override fun getName() = "Cobol File"
    override fun getDescription() = "Cobol File"

    override fun getDefaultExtension() = "cbl"

    override fun getIcon(): Icon = Companion.icon
    companion object {
        val INSTANCE = CobolFileType()
        val icon = IconLoader.getIcon("/icons/icon.png", CobolFileType::class.java)
    }
}
