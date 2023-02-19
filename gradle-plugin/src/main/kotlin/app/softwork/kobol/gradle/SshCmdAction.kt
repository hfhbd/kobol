package app.softwork.kobol.gradle

import com.jcraft.jsch.agentproxy.*
import com.jcraft.jsch.agentproxy.connector.*
import com.jcraft.jsch.agentproxy.sshj.*
import net.schmizz.sshj.*
import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.logging.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.work.*
import org.gradle.workers.*
import javax.inject.*

@DisableCachingByDefault
public abstract class SshCmdTask : DefaultTask() {
    @get:Input
    public abstract val host: Property<String>

    @get:Input
    public abstract val user: Property<String>

    @get:Input
    public abstract val folder: Property<String>

    @get:Input
    public abstract val cmds: ListProperty<String>

    @get:Input
    public open val export: String = "export PATH=${"$"}{PATH}:${"$"}{JAVA_HOME}/bin"

    init {
        group = "kobol"
        folder.convention(project.name)
    }

    @get:Internal
    public val configuration: String = project.configurations.register("${name}Ssh") {
        dependencies.add(project.dependencies.create("app.softwork.kobol:ssh-env:$kobolVersion"))
    }.name

    @get:InputFiles
    @get:Classpath
    internal val sshClasspath: FileCollection =
        project.objects.fileCollection().from(project.configurations.named(configuration))


    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @TaskAction
    internal fun execute() {
        workerExecutor.classLoaderIsolation {
            classpath.setFrom(sshClasspath)
        }.submit(SshCmdAction::class.java) {
            host.set(this@SshCmdTask.host)
            user.set(this@SshCmdTask.user)
            folder.set(this@SshCmdTask.folder)
            cmds.set(this@SshCmdTask.cmds)
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
            val export = parameters.export.get().joinToString(separator = ":", prefix = "export ")
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
        val proxy = AgentProxy((PageantConnector()))
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
