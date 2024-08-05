package app.softwork.kobol.gradle

import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.api.tasks.PathSensitivity.*
import org.gradle.kotlin.dsl.submit
import org.gradle.work.*

@CacheableTask
public abstract class UploadTask : SshTask() {
    @get:Incremental
    @get:PathSensitive(RELATIVE)
    @get:InputFiles
    public abstract val files: ConfigurableFileCollection

    @get:Optional
    @get:Input
    public abstract val encoding: Property<String>

    init {
        encoding.convention("ibm-1047")
    }

    @get:Optional
    @get:Input
    public abstract val mvsFolder: Property<String>

    @get:Optional
    @get:Input
    public abstract val keepUTF8: Property<Boolean>

    init {
        keepUTF8.convention(false)
    }

    @get:Optional
    @get:PathSensitive(RELATIVE)
    @get:InputFiles
    public abstract val mvsFiles: ConfigurableFileCollection

    @get:OutputDirectory
    public abstract val uploaded: DirectoryProperty

    init {
        uploaded.convention(project.layout.buildDirectory.dir("kobol/uploaded"))
    }

    @get:Input
    @get:Optional
    public abstract val notTagged: ListProperty<String>

    init {
        notTagged.convention(listOf("jar", "class", "jks"))
    }

    @TaskAction
    internal fun execute(inputChanges: InputChanges) {
        val queue = workerExecutor.classLoaderIsolation {
            classpath.setFrom(sshClasspath)
        }

        for (change in inputChanges.getFileChanges(files)) {
            if (change.fileType == FileType.DIRECTORY) continue
            val changeType: ChangeType = change.changeType

            when (changeType) {
                ChangeType.ADDED, ChangeType.MODIFIED -> {
                    queue.submit(UploadAction::class) {
                        user.set(this@UploadTask.user)
                        host.set(this@UploadTask.host)
                        folder.set(this@UploadTask.folder)
                        file.set(change.file)
                        encoding.set(this@UploadTask.encoding)
                        mvsFolder.set(this@UploadTask.mvsFolder)
                        keepUTF8.set(this@UploadTask.keepUTF8)
                        mvsFiles.from(this@UploadTask.mvsFiles)
                        uploaded.set(this@UploadTask.uploaded)
                        notTagged.set(this@UploadTask.notTagged)
                    }
                }

                ChangeType.REMOVED -> {
                    queue.submit(DeleteAction::class) {
                        user.set(this@UploadTask.user)
                        host.set(this@UploadTask.host)
                        folder.set(this@UploadTask.folder)
                        file.set(change.file)
                    }
                }
            }
        }
    }
}
