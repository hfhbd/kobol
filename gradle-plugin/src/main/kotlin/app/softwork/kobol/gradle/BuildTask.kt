package app.softwork.kobol.gradle

import org.gradle.work.*

@DisableCachingByDefault
public abstract class BuildTask : SshCmdTask() {
    init {
        export.set(listOf("PATH=${"$"}{PATH}:/usr/lpp/IBM/cobol/igyv6r3/bin"))
    }
}
