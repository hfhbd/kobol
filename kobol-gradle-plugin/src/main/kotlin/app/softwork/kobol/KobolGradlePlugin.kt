package app.softwork.kobol

import org.gradle.api.*
import org.gradle.api.tasks.*

class KobolGradlePlugin : Plugin<Project> {
    override fun apply(target: Project) {
        target.tasks.register("convertCobolToKotlin", KobolTask::class.java)
        val upload = target.tasks.register("uploadCobol", UploadTask::class.java) {
            it.group = "kobol"
        }
        target.tasks.register("buildCobol", BuildTask::class.java) {
            it.dependsOn(upload)
            it.group = "kobol"
        }
        target.plugins.withId("org.jetbrains.kotlin.jvm") {
            val srcSet = target.extensions.findByType(SourceSetContainer::class.java)!!.getByName("main")
            val kotlin = srcSet.extensions.getByName("kotlin") as org.gradle.api.file.SourceDirectorySet
            kotlin.srcDir("build/generated/kobol")
        }
    }
}
