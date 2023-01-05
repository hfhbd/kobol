package app.softwork.kobol.gradle

import org.gradle.api.*
import org.gradle.api.tasks.TaskAction

public abstract class CleanCobol: DefaultTask(), SshTask {
    @TaskAction
    public fun clean() {
        sshClient { 
            exec("cd ..; rm ${folder.get()}")
        }
    }
}
