package app.softwork.kobol.gradle

import net.schmizz.sshj.xfer.*
import org.gradle.api.file.*
import org.gradle.api.logging.*
import org.gradle.api.provider.*
import org.gradle.workers.*
import java.io.*

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
