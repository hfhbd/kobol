package app.softwork.kobol

import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.work.*

@DisableCachingByDefault
abstract class BuildTask : AbstractExecTask<BuildTask>(BuildTask::class.java), SshTask {
    @get:Input
    abstract val cmds: ListProperty<String>

    override fun exec() {
        val workdir = folder.get()
        sshClient {
            for (cmd: String in cmds.get()) {
                exec("""cd $workdir; export JAVA_HOME=/usr/lpp/java/current; export PATH=${"$"}PATH:/usr/lpp/IBM/cobol/igyv6r3/bin; $cmd""")
            }
        }
    }
}
