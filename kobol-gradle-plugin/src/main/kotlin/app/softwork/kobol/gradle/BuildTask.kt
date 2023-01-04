package app.softwork.kobol.gradle

import org.gradle.work.*

@DisableCachingByDefault
public abstract class BuildTask : KobolRunTask() {
    override val export: String = "export PATH=${"$"}PATH:/usr/lpp/IBM/cobol/igyv6r3/bin;"
}
