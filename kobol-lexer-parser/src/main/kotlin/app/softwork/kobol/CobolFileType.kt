package app.softwork.kobol

import com.intellij.openapi.fileTypes.*
import com.intellij.openapi.util.*
import javax.swing.*

public class CobolFileType private constructor(): LanguageFileType(CobolLanguage) {
    override fun getName(): String = "Cobol File"
    override fun getDescription(): String = "Cobol File"

    override fun getDefaultExtension(): String = "cbl"

    override fun getIcon(): Icon = Companion.icon
    public companion object {
        @JvmStatic
        public val INSTANCE: CobolFileType = CobolFileType()

        public val icon: Icon = IconLoader.getIcon("/icons/icon.png", CobolFileType::class.java)
    }
}
