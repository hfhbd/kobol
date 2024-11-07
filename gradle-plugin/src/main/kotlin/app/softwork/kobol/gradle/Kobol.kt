package app.softwork.kobol.gradle

import org.gradle.api.Action
import org.gradle.api.Named
import org.gradle.api.NamedDomainObjectContainer
import org.gradle.api.tasks.Nested

interface Kobol {
    @get:Nested
    val dependencies: KobolDependencies

    fun dependencies(action: Action<KobolDependencies>) {
        action.execute(dependencies)
    }

    val firActions: NamedDomainObjectContainer<KobolFirSpec>
}

interface KobolFirSpec : Named {
    @get:Nested
    val dependencies: KobolDependencies

    fun dependencies(action: Action<KobolDependencies>) {
        action.execute(dependencies)
    }
}
