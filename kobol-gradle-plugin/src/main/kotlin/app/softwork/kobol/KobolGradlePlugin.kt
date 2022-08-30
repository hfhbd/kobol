package app.softwork.kobol

import org.gradle.api.*

class KobolGradlePlugin : Plugin<Project> {
    override fun apply(target: Project) {
        val convert = target.tasks.register("convertCobolToKotlin", KobolTask::class.java)
        target.plugins.withId("org.jetbrains.kotlin.jvm") {
            target.tasks.getByName("compileKotlin").dependsOn(convert)
        }

        val upload = target.tasks.register("uploadCobol", UploadTask::class.java) {
            it.group = "kobol"
        }
        target.tasks.register("buildCobol", BuildTask::class.java) {
            it.dependsOn(upload)
            it.group = "kobol"
        }
    }
}
