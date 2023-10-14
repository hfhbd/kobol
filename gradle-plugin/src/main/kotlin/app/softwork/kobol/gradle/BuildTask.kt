package app.softwork.kobol.gradle

import org.gradle.api.provider.*
import org.gradle.api.tasks.*
import org.gradle.work.*

@DisableCachingByDefault
public abstract class BuildTask : KobolRunTask() {
    @get:Input
    public abstract val c89Options: Property<String>

    @get:Input
    public abstract val c89: ListProperty<String>

    @get:Input
    public abstract val cob2Options: Property<String>

    @get:Input
    public abstract val cob2: ListProperty<String>

    private fun Provider<List<String>>.options(options: Provider<String>): Provider<List<String>> =
        zip(options) { list, options ->
            list.map {
                "$options $it"
            }
        }

    private operator fun <T : Any> Provider<List<T>>.plus(other: Provider<List<T>>): Provider<List<T>> =
        zip(other) { a, b ->
            a + b
        }

    init {
        val c89cmd = c89.options(c89Options)
        val cob2cmd = cob2.options(cob2Options)
        cmds.convention(c89cmd + cob2cmd)
    }
}
