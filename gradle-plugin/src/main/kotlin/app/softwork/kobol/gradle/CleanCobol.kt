package app.softwork.kobol.gradle

import org.gradle.api.file.*
import org.gradle.api.tasks.*

@CacheableTask
public abstract class CleanCobol : SshCmdTask() {
    @get:Destroys
    public abstract val uploaded: DirectoryProperty

    @TaskAction
    public fun clean() {
        val queue = workerExecutor.classLoaderIsolation {
            classpath.setFrom(sshClasspath)
        }
        queue.submit(SshCmdAction::class.java) {
            host.set(this@CleanCobol.host)
            user.set(this@CleanCobol.user)
            folder.set(this@CleanCobol.folder)
            cmds.set(this@CleanCobol.cmds)
        }
        queue.await()
        uploaded.get().asFile.deleteRecursively()
    }
}
