package app.softwork.kobol.gradle

import org.gradle.api.file.*
import org.gradle.api.tasks.*
import org.gradle.kotlin.dsl.submit
import org.gradle.work.DisableCachingByDefault

@DisableCachingByDefault(because = "@Destroys does not support caching")
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
            classpath.setFrom(sshClasspath)
        }
        queue.submit(SshCmdAction::class) {
            host.set(this@CleanCobol.host)
            user.set(this@CleanCobol.user)
            folder.set(this@CleanCobol.folder)
            cmds.set(this@CleanCobol.cmds)
        }
        queue.await()
        uploaded.get().asFile.deleteRecursively()
    }
}
