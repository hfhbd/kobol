package app.softwork.kobol.intellij

import app.softwork.kobol.CobolFileType
import app.softwork.kobol.intellij.CobolLanguageCodeStyleSettingsProvider.Companion.cobolSampleCode
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.options.colors.AttributesDescriptor
import com.intellij.openapi.options.colors.ColorDescriptor
import com.intellij.openapi.options.colors.ColorSettingsPage
import javax.swing.Icon

internal class CobolColorSettingsPage : ColorSettingsPage {
    override fun getIcon(): Icon = CobolFileType.icon

    override fun getHighlighter() = CobolSyntaxHighlighter

    override fun getAdditionalHighlightingTagToDescriptorMap(): Map<String, TextAttributesKey>? = null

    override fun getDemoText(): String = cobolSampleCode

    override fun getAttributeDescriptors(): Array<AttributesDescriptor> = DESCRIPTORS

    override fun getColorDescriptors(): Array<ColorDescriptor> = ColorDescriptor.EMPTY_ARRAY

    override fun getDisplayName() = "Cobol"

    companion object {
        private val DESCRIPTORS: Array<AttributesDescriptor> = arrayOf<AttributesDescriptor>(
            AttributesDescriptor("Comment", CobolSyntaxHighlighter.comment),
            AttributesDescriptor("Bad character", CobolSyntaxHighlighter.badCharacter),
            AttributesDescriptor("Vars", CobolSyntaxHighlighter.vars),
            AttributesDescriptor("Function", CobolSyntaxHighlighter.function),
            AttributesDescriptor("String", CobolSyntaxHighlighter.string),
            AttributesDescriptor("Keyword", CobolSyntaxHighlighter.keyword),
            AttributesDescriptor("Dot", CobolSyntaxHighlighter.dot),
            AttributesDescriptor("Number", CobolSyntaxHighlighter.number),
        )
    }
}
