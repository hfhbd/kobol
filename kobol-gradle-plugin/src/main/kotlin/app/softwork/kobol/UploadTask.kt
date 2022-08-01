package app.softwork.kobol

import com.jcraft.jsch.agentproxy.*
import com.jcraft.jsch.agentproxy.connector.*
import net.schmizz.sshj.*
import net.schmizz.sshj.xfer.FileSystemFile
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.internal.file.copy.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.workers.internal.*

abstract class UploadTask : AbstractCopyTask() {
    @get:Input
    abstract val host: Property<String>

    @get:Input
    abstract val user: Property<String>

    @get:Input
    abstract val folder: Property<String>

    override fun createCopyAction() = Upload()

    inner class Upload : CopyAction {
        override fun execute(stream: CopyActionProcessingStream): WorkResult {
            val ssh = SSHClient()
            ssh.connect(host.get())
            val proxy = AgentProxy((PageantConnector()))
            ssh.auth(user.get(), com.jcraft.jsch.agentproxy.sshj.AuthAgent(proxy, proxy.identities.first()))
            val sftp = ssh.newSFTPClient()
            stream.process {
                sftp.put(FileSystemFile(it.file), "${folder.get()}/${it.file.name}")
            }
            return DefaultWorkResult.SUCCESS
        }
    }
}
