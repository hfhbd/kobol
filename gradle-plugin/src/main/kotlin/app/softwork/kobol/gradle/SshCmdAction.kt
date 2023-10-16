package app.softwork.kobol.gradle

import org.gradle.api.logging.*
import org.gradle.api.provider.*
import org.gradle.workers.*

internal abstract class SshCmdAction : WorkAction<SshCmdAction.Parameters> {
    internal interface Parameters : SshParameters {
        val cmds: ListProperty<String>
        val export: ListProperty<String>
    }

    private val logger = Logging.getLogger(SshCmdAction::class.java)

    override fun execute() {
        sshClient(host = parameters.host.get(), user = parameters.user.get()) {
            val workdir = parameters.folder.get()
            val export = parameters.export.get().takeUnless { it.isEmpty() }
            val exportJoined = export?.joinToString(prefix = "export ", separator = "; export ", postfix = ";") ?: ""
            for (cmd: String in parameters.cmds.get()) {
                exec("""cd $workdir; $exportJoined $cmd""", logger = logger)
            }
        }
    }
}
