package app.softwork.kobol.gradle

import com.jcraft.jsch.agentproxy.*
import com.jcraft.jsch.agentproxy.connector.*
import com.jcraft.jsch.agentproxy.sshj.*
import net.schmizz.sshj.*
import org.gradle.api.logging.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.work.*
import org.gradle.workers.*

@DisableCachingByDefault
public abstract class KobolRunTask : SshTask() {
    @get:Input
    public abstract val cmds: ListProperty<String>

    @get:Input
    public abstract val export: ListProperty<String>

    init {
        export.convention(listOf("PATH=${"$"}{PATH}:${"$"}{JAVA_HOME}/bin"))
    }

    @TaskAction
    internal fun execute() {
        workerExecutor.classLoaderIsolation {
            classpath.setFrom(sshClasspath)
        }.submit(SshCmdAction::class.java) {
            host.set(this@KobolRunTask.host)
            user.set(this@KobolRunTask.user)
            folder.set(this@KobolRunTask.folder)
            cmds.set(this@KobolRunTask.cmds)
            export.set(this@KobolRunTask.export)
        }
    }
}

internal abstract class SshCmdAction : WorkAction<SshCmdAction.Parameters> {
    internal interface Parameters : SshParameters {
        public val cmds: ListProperty<String>
        public val export: ListProperty<String>
    }

    private val logger = Logging.getLogger(SshCmdAction::class.java)

    override fun execute() {
        sshClient(host = parameters.host.get(), user = parameters.user.get()) {
            val workdir = parameters.folder.get()
            val export = parameters.export.get().joinToString(prefix = "export ", separator = "; export", postfix = ";")
            for (cmd: String in parameters.cmds.get()) {
                exec("""cd $workdir; export JAVA_HOME=/usr/lpp/java/current; $export $cmd""", logger = logger)
            }
        }
    }
}

internal fun sshClient(host: String, user: String, action: SSHClient.() -> Unit) {
    SSHClient().use { ssh ->
        ssh.loadKnownHosts()
        ssh.connect(host)
        val proxy = AgentProxy(PageantConnector())
        ssh.auth(user, proxy.identities.map { AuthAgent(proxy, it) })
        ssh.action()
    }
}

internal fun SSHClient.exec(cmd: String, logger: Logger) {
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

internal interface SshParameters : WorkParameters {
    val host: Property<String>
    val user: Property<String>
    val folder: Property<String>
}
