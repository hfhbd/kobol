package app.softwork.kobol

import com.jcraft.jsch.agentproxy.*
import com.jcraft.jsch.agentproxy.connector.*
import com.jcraft.jsch.agentproxy.sshj.*
import net.schmizz.sshj.*
import org.gradle.api.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*

interface SshTask : Task {
    @get:Input
    val host: Property<String>

    @get:Input
    val user: Property<String>

    @get:Input
    val folder: Property<String>

    fun sshClient(action: SSHClient.() -> Unit) {
        SSHClient().use { ssh ->
            ssh.loadKnownHosts()
            ssh.connect(host.get())
            val proxy = AgentProxy((PageantConnector()))
            ssh.auth(user.get(), proxy.identities.map { AuthAgent(proxy, it) })
            ssh.action()
        }
    }

    fun SSHClient.exec(cmd: String) {
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
