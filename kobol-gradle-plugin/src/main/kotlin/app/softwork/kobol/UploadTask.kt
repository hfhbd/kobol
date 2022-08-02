package app.softwork.kobol

import net.schmizz.sshj.xfer.*
import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.work.*

@DisableCachingByDefault
abstract class UploadTask : DefaultTask(), SshTask {
    @get:InputFiles
    abstract val files: ConfigurableFileCollection

    @get:Input
    abstract val encoding: Property<String>

    init {
        encoding.convention("ibm-1047")
    }

    @TaskAction
    fun execute() {
        val encoding = encoding.get()
        sshClient {
            newSFTPClient().use { sftp ->
                for (file in files.files) {
                    val folder = folder.get()
                    sftp.mkdirs(folder)

                    val target = "$folder/${file.name}"

                    sftp.put(FileSystemFile(file), target)
                    exec("iconv -T -f utf-8 -t $encoding $target > $target.conv")
                    exec("mv $target.conv $target")
                }
            }
        }
    }
}
