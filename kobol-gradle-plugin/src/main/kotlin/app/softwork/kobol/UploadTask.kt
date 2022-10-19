package app.softwork.kobol

import net.schmizz.sshj.xfer.*
import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.api.tasks.PathSensitivity.*
import org.gradle.work.*
import java.io.*

@CacheableTask
abstract class UploadTask : DefaultTask(), SshTask {
    init {
        group = "kobol"
    }

    @get:Incremental
    @get:PathSensitive(RELATIVE)
    @get:InputFiles
    abstract val files: ConfigurableFileCollection

    @get:Optional
    @get:Input
    abstract val encoding: Property<String>

    init {
        encoding.convention("ibm-1047")
    }

    @get:Optional
    @get:Input
    abstract val mvsFolder: Property<String>

    @get:Optional
    @get:Input
    abstract val keepUTF8: Property<Boolean>

    init {
        keepUTF8.convention(false)
    }

    @get:Optional
    @get:PathSensitive(RELATIVE)
    @get:InputFiles
    abstract val mvsFiles: ConfigurableFileCollection

    @get:OutputDirectory
    abstract val uploaded: DirectoryProperty

    init {
        uploaded.convention(project.layout.buildDirectory.dir("kobol/uploaded"))
    }

    @get:Input
    @get:Optional
    abstract val notTagged: ListProperty<String>

    init {
        notTagged.convention(listOf("jar"))
    }

    @TaskAction
    fun execute(inputChanges: InputChanges) {
        val encoding = encoding.get()
        val copyToMVS = mvsFolder.orNull
        val keepUTF8 = keepUTF8.get()
        val folder = folder.get()
        val uploaded = uploaded.get().asFile
        val notTagged: List<String> = notTagged.get()

        sshClient {
            newSFTPClient().use { sftp ->
                for (change in inputChanges.getFileChanges(files)) {
                    if (change.fileType == FileType.DIRECTORY) continue

                    sftp.mkdirs(folder)

                    val file = change.file
                    val target = "$folder/${file.name}"

                    try {
                        sftp.put(FileSystemFile(file), target)
                    } catch (_: IOException) {
                        sftp.put(FileSystemFile(file), target)
                    }
                    if (file.extension !in notTagged) {
                        exec("iconv -T -f utf-8 -t $encoding $target > $target.conv")

                        if (keepUTF8) {
                            exec("mv $target $target.utf8")
                        }
                        exec("mv $target.conv $target")
                    }
                    if (copyToMVS != null && file in mvsFiles) {
                        val mvsName = file.nameWithoutExtension.uppercase()
                        exec("""cp $target "//'$copyToMVS($mvsName)'" """)
                    }
                    file.copyTo(File(uploaded, file.name), true)
                }
            }
        }
    }
}
