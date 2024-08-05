package app.softwork.kobol.gradle

import org.gradle.api.Action
import org.gradle.api.tasks.Nested

interface Kobol {
    @get:Nested
    val dependencies: KobolDependencies

    fun dependencies(action: Action<KobolDependencies>) {
        action.execute(dependencies)
    }
}
