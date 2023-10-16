package app.softwork.kobol.gradle

import com.jcraft.jsch.agentproxy.*
import net.schmizz.sshj.*
import org.gradle.api.logging.*

internal fun sshClient(host: String, user: String, action: SSHClient.() -> Unit) {
    SSHClient().use { ssh ->
        ssh.loadKnownHosts()
        ssh.connect(host)
        ssh.auth(user, PAgentProxy.authAgents())
        ssh.action()
    }
}

internal fun SSHClient.exec(cmd: String, logger: Logger) {
    logger.info(cmd)
    val (output, result) = startSession().use { session ->
        val result = session.exec(cmd)
        result.inputStream.reader().use { it.readText() } to result
    }
    val exitCode: Int? = result.exitStatus
    if (exitCode != 0) {
        val error = result.errorStream.reader().use { it.readText() }
        logger.error("$cmd\n$error " + (result.exitErrorMessage ?: ""))
    }
    if (output.isNotBlank()) {
        logger.quiet(output)
    }
}
