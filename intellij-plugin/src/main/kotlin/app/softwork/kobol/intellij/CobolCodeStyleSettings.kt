package app.softwork.kobol.intellij

import com.intellij.psi.codeStyle.CodeStyleSettings
import com.intellij.psi.codeStyle.CustomCodeStyleSettings

class CobolCodeStyleSettings(settings: CodeStyleSettings) : CustomCodeStyleSettings(
    "CobolCodeStyleSettings",
    settings,
) {
    init {
        settings.defaultSoftMargins = listOf(6, 7, 12, 80)
    }
}
