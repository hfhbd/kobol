package app.softwork.kobol

import net.schmizz.sshj.xfer.*
import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.api.tasks.PathSensitivity.*
import org.gradle.work.*

@DisableCachingByDefault
abstract class UploadTask : DefaultTask(), SshTask {
    @get:PathSensitive(RELATIVE)
    @get:InputFiles
    abstract val files: ConfigurableFileCollection

    @get:Optional
    @get:Input
    abstract val encoding: Property<String>

    @get:Optional
    @get:Input
    abstract val mvsFolder: Property<String>

    @get:Optional
    @get:Input
    abstract val keepUTF8: Property<Boolean>

    @get:Optional
    @get:PathSensitive(RELATIVE)
    @get:InputFiles
    abstract val mvsFiles: ConfigurableFileCollection

    @TaskAction
    fun execute() {
        val encoding = encoding.orNull ?: "ibm-1047"
        val copyToMVS = mvsFolder.orNull
        val keepUTF8 = keepUTF8.orNull ?: false
        sshClient {
            newSFTPClient().use { sftp ->
                for (file in files) {
                    val folder = folder.get()
                    sftp.mkdirs(folder)

                    val target = "$folder/${file.name}"

                    sftp.put(FileSystemFile(file), target)
                    exec("iconv -T -f utf-8 -t $encoding $target > $target.conv")
                    if (keepUTF8) {
                        exec("mv $target $target.utf8")
                    }
                    exec("mv $target.conv $target")
                    if (copyToMVS != null && file in mvsFiles) {
                        val mvsName = file.nameWithoutExtension.uppercase()
                        exec("""cp $target "//'$copyToMVS($mvsName)'" """)
                    }
                }
            }
        }
    }
}
