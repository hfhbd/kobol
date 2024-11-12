package app.softwork.kobol.intellij

import app.softwork.kobol.CobolLanguage
import com.intellij.psi.codeStyle.LanguageCodeStyleSettingsProvider
import com.intellij.psi.codeStyle.LanguageCodeStyleSettingsProvider.SettingsType

internal class CobolLanguageCodeStyleSettingsProvider : LanguageCodeStyleSettingsProvider() {
    override fun getLanguage() = CobolLanguage

    override fun getCodeSample(settingsType: SettingsType): String {
        // language=cobol
        return """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WO-RLD PIC X(6) VALUE 'WORLD!'.
        123456 PROCEDURE                   DIVISION.
        123456
        123456* Some Comment
        123456     DISPLAY "HELLO " WO-RLD
        123456     MOVE "42" TO WO-RLD
        123456     DISPLAY WO-RLD
        123456     DISPLAY "ANSWER"WO-RLD.
        """.trimIndent()
    }
}
