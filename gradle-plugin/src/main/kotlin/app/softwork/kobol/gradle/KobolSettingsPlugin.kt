package app.softwork.kobol.gradle

import org.gradle.api.Plugin
import org.gradle.api.initialization.Settings
import org.gradle.api.internal.plugins.software.RegistersSoftwareTypes

@RegistersSoftwareTypes(KobolPlugin::class)
public abstract class KobolSettingsPlugin : Plugin<Settings> {
    override fun apply(target: Settings) {}
}
