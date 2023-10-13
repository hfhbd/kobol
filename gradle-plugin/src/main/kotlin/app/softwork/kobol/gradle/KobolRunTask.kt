package app.softwork.kobol.gradle

import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.work.*

@DisableCachingByDefault
public abstract class KobolRunTask : SshTask() {
    @get:Input
    public abstract val cmds: ListProperty<String>

    @get:Input
    public abstract val export: ListProperty<String>

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
