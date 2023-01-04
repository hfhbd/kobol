package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.work.*

@DisableCachingByDefault
public abstract class KobolRunTask : DefaultTask(), SshTask {
    init {
        group = "kobol"
    }

    internal open val export: String = ""

    @get:Input
    public abstract val cmds: ListProperty<String>

    @TaskAction
    internal fun exec() {
        val workdir = folder.get()
        sshClient {
            for (cmd: String in cmds.get()) {
                exec("""cd $workdir; export JAVA_HOME=/usr/lpp/java/current; $export $cmd""")
            }
        }
    }
}
