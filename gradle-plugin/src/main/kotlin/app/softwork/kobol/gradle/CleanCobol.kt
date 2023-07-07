package app.softwork.kobol.gradle

import org.gradle.api.file.*
import org.gradle.api.tasks.*

@CacheableTask
public abstract class CleanCobol : KobolRunTask() {
    @get:Destroys
    public abstract val uploaded: DirectoryProperty

    init {
        cmds.set(folder.map { listOf("cd ..; rm $it") })
        cmds.finalizeValue()
    }

    @TaskAction
    public fun clean() {
        val queue = workerExecutor.classLoaderIsolation {
            it.classpath.setFrom(sshClasspath)
        }
        queue.submit(SshCmdAction::class.java) {
            it.host.set(host)
            it.user.set(user)
            it.folder.set(folder)
            it.cmds.set(cmds)
        }
        queue.await()
        uploaded.get().asFile.deleteRecursively()
    }
}
