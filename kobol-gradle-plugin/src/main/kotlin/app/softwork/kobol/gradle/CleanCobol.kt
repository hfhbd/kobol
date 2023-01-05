package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.tasks.*

@CacheableTask
public abstract class CleanCobol : DefaultTask(), SshTask {
    init {
        group = "kobol"
    }

    @TaskAction
    public fun clean() {
        sshClient {
            exec("cd ..; rm ${folder.get()}")
        }
    }
}
