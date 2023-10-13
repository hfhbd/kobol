package app.softwork.kobol.gradle

import org.gradle.api.file.*
import org.gradle.workers.*

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
