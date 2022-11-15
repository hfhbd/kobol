package app.softwork.kobol.gradle

import com.jcraft.jsch.agentproxy.*
import com.jcraft.jsch.agentproxy.connector.*
import com.jcraft.jsch.agentproxy.sshj.*
import net.schmizz.sshj.*
import org.gradle.api.Task
import org.gradle.api.provider.*
import org.gradle.api.tasks.*

public interface SshTask : Task {
    @get:Input
    public val host: Property<String>

    @get:Input
    public val user: Property<String>

    @get:Input
    public val folder: Property<String>

    public fun sshClient(action: SSHClient.() -> Unit) {
        SSHClient().use { ssh ->
            ssh.loadKnownHosts()
            ssh.connect(host.get())
            val proxy = AgentProxy((PageantConnector()))
            ssh.auth(user.get(), proxy.identities.map { AuthAgent(proxy, it) })
            ssh.action()
        }
    }

    public fun SSHClient.exec(cmd: String) {
        val (output, result) = startSession().use {
            val result = it.exec(cmd)
            result.inputStream.reader().use { it.readText() } to result
        }
        val exitCode: Int? = result.exitStatus
        require(exitCode == 0) {
            val error = result.errorStream.reader().use { it.readText() }
            "$cmd\n$error " + (result.exitErrorMessage ?: "")
        }
        if (output.isNotBlank()) {
            logger.quiet("$cmd\n$output")
        }
    }
}
