package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.file.*
import org.gradle.api.tasks.*

@CacheableTask
public abstract class CleanCobol : DefaultTask(), SshTask {
    init {
        group = "kobol"
    }

    @get:Destroys
    public abstract val uploaded: DirectoryProperty

    @TaskAction
    public fun clean() {
        sshClient {
            exec("cd ..; rm ${folder.get()}")
        }
        uploaded.get().asFile.deleteRecursively()
    }
}
