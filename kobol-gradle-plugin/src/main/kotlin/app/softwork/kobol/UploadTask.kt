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

    @get:Input
    abstract val encoding: Property<String>

    @get:Input
    abstract val mvsFolder: Property<String>

    @get:PathSensitive(RELATIVE)
    @get:InputFiles
    abstract val mvsFiles: ConfigurableFileCollection

    init {
        encoding.convention("ibm-1047")
    }

    @TaskAction
    fun execute() {
        val encoding = encoding.get()
        val copyToMVS = mvsFolder.orNull
        sshClient {
            newSFTPClient().use { sftp ->
                for (file in files) {
                    val folder = folder.get()
                    sftp.mkdirs(folder)

                    val target = "$folder/${file.name}"

                    sftp.put(FileSystemFile(file), target)
                    exec("iconv -T -f utf-8 -t $encoding $target > $target.conv")
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
