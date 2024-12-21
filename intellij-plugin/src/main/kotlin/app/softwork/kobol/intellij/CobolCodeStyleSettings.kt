package app.softwork.kobol.intellij

import com.intellij.psi.codeStyle.CodeStyleSettings
import com.intellij.psi.codeStyle.CustomCodeStyleSettings

class CobolCodeStyleSettings(settings: CodeStyleSettings) : CustomCodeStyleSettings(
    "CobolCodeStyleSettings",
    settings,
)
