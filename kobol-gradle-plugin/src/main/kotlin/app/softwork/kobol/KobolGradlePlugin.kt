package app.softwork.kobol

import org.gradle.api.*

class KobolGradlePlugin: Plugin<Project> {
    override fun apply(target: Project) {
        target.tasks.register("convertCobolToKotlin", KobolTask::class.java)
        val upload = target.tasks.register("uploadCobol", UploadTask::class.java) {
            it.group = "kobol"
        }
        target.tasks.register("buildCobol", BuildTask::class.java) {
            it.dependsOn(upload)
            it.group = "kobol"
        }
    }
}
