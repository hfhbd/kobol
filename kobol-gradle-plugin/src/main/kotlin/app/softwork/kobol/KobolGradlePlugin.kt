package app.softwork.kobol

import org.gradle.api.*

class KobolGradlePlugin: Plugin<Project> {
    override fun apply(target: Project) {
        target.tasks.register("convertCobolToKotlin", KobolTask::class.java)
        target.tasks.register("upload", UploadTask::class.java)
    }
}
