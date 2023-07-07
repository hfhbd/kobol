package app.softwork.kobol.gradle

import net.schmizz.sshj.xfer.*
import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.logging.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.api.tasks.PathSensitivity.*
import org.gradle.work.*
import org.gradle.workers.*
import java.io.*
import javax.inject.*

@DisableCachingByDefault
public abstract class SshTask : DefaultTask() {
    init {
        group = "kobol"
        folder.convention(project.name)
    }

    @get:Input
    public abstract val host: Property<String>

    @get:Input
    public abstract val user: Property<String>

    @get:Input
    public abstract val folder: Property<String>

    @get:Internal
    public val configuration: String = project.configurations.register("${name}Ssh") {
        dependencies.add(project.dependencies.create("app.softwork.kobol:ssh-env:$KOBOL_VERSION"))
    }.name

    @get:InputFiles
    @get:Classpath
    internal val sshClasspath: FileCollection =
        project.objects.fileCollection().from(project.configurations.named(configuration))


    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor
}

@CacheableTask
public abstract class UploadTask : SshTask() {
    @get:Incremental
    @get:PathSensitive(RELATIVE)
    @get:InputFiles
    public abstract val files: ConfigurableFileCollection

    public fun files(vararg cobols: NamedDomainObjectProvider<CobolSource>) {
        files.from(cobols.map { it.file })
    }

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
        notTagged.convention(listOf("jar", "class"))
    }

    @TaskAction
    internal fun execute(inputChanges: InputChanges) {
        val queue = workerExecutor.classLoaderIsolation {
            classpath.setFrom(sshClasspath)
        }

        for (change in inputChanges.getFileChanges(files)) {
            if (change.fileType == FileType.DIRECTORY) continue

            when (change.changeType) {
                ChangeType.ADDED, ChangeType.MODIFIED -> {
                    queue.submit(UploadAction::class.java) {
                        user.set(this@UploadTask.user)
                        host.set(this@UploadTask.host)
                        this.folder.set(this@UploadTask.folder)
                        this.file.set(change.file)
                        this.encoding.set(this@UploadTask.encoding)
                        this.mvsFolder.set(this@UploadTask.mvsFolder)
                        this.keepUTF8.set(this@UploadTask.keepUTF8)
                        this.mvsFiles.from(this@UploadTask.mvsFiles)
                        this.uploaded.set(this@UploadTask.uploaded)
                        this.notTagged.set(this@UploadTask.notTagged)
                    }
                }

                ChangeType.REMOVED -> {
                    queue.submit(DeleteAction::class.java) {
                        user.set(this@UploadTask.user)
                        host.set(this@UploadTask.host)
                        this.folder.set(this@UploadTask.folder)
                        this.file.set(change.file)
                    }
                }
            }
        }
    }
}

internal abstract class DeleteAction : WorkAction<DeleteAction.Parameters> {
    interface Parameters : SshParameters {
        val file: RegularFileProperty
    }

    override fun execute() {
        val folder = parameters.folder.get()
        val file = parameters.file.asFile.get()
        val target = "$folder/${file.name}"
        sshClient(host = parameters.host.get(), user = parameters.user.get()) {
            newSFTPClient().use { sftp ->
                sftp.rm(target)
            }
        }
        file.delete()
    }
}

internal abstract class UploadAction : WorkAction<UploadAction.Parameters> {
    interface Parameters : SshParameters {
        val file: RegularFileProperty
        val encoding: Property<String>
        val mvsFolder: Property<String>
        val keepUTF8: Property<Boolean>
        val mvsFiles: ConfigurableFileCollection
        val uploaded: DirectoryProperty
        val notTagged: ListProperty<String>
    }

    private val logger = Logging.getLogger(UploadAction::class.java)

    override fun execute() {
        sshClient(user = parameters.user.get(), host = parameters.host.get()) {
            newSFTPClient().use { sftp ->

                val folder = parameters.folder.get()
                sftp.mkdirs(folder)
                val file = parameters.file.asFile.get()
                val target = "$folder/${file.name}"

                try {
                    sftp.put(FileSystemFile(file), target)
                } catch (_: IOException) {
                    sftp.put(FileSystemFile(file), target)
                }
                if (file.extension !in parameters.notTagged.get()) {
                    exec("iconv -T -f utf-8 -t ${parameters.encoding.get()} $target > $target.conv", logger)

                    if (parameters.keepUTF8.get()) {
                        exec("mv $target $target.utf8", logger)
                    }
                    exec("mv $target.conv $target", logger)
                }
                val copyToMVS: String? = parameters.mvsFolder.orNull
                if (copyToMVS != null && file in parameters.mvsFiles) {
                    val mvsName = file.nameWithoutExtension.uppercase()
                    exec("""cp $target "//'$copyToMVS($mvsName)'" """, logger)
                }
                file.copyTo(File(parameters.uploaded.asFile.get(), file.name), true)
            }
        }
    }
}
