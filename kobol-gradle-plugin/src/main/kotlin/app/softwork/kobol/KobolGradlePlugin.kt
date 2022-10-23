package app.softwork.kobol

import org.gradle.api.*

class KobolGradlePlugin : Plugin<Project> {
    override fun apply(target: Project) {
        target.plugins.apply("org.gradle.idea")
        val convert = target.tasks.register("convertCobolToKotlin", KobolTask::class.java)
        val convertJava = target.tasks.register("convertCobolToJava", JavaKobolTask::class.java)
        target.plugins.withId("org.jetbrains.kotlin.jvm") {
            target.tasks.getByName("compileKotlin").dependsOn(convert, convertJava)
        }

        val upload = target.tasks.register("uploadCobol", UploadTask::class.java)
        target.tasks.register("buildCobol", BuildTask::class.java) {
            it.dependsOn(upload)
        }
    }
}
