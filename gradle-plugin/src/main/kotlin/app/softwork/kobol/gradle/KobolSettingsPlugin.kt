package app.softwork.kobol.gradle

import org.gradle.api.Plugin
import org.gradle.api.initialization.Settings

public abstract class KobolSettingsPlugin : Plugin<Settings> {
    override fun apply(target: Settings): Unit = Unit
}
