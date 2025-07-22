package app.softwork.kobol.gradle

import org.gradle.api.Action
import org.gradle.api.Named
import org.gradle.api.NamedDomainObjectContainer
import org.gradle.api.tasks.Nested
import org.gradle.declarative.dsl.model.annotations.Configuring

public interface Kobol {
    @get:Nested
    public val dependencies: KobolDependencies

    @Configuring
    public fun dependencies(action: Action<KobolDependencies>) {
        action.execute(dependencies)
    }

    public val firActions: NamedDomainObjectContainer<KobolFirSpec>
}

public interface KobolFirSpec : Named {
    @get:Nested
    public val dependencies: KobolDependencies

    @Configuring
    public fun dependencies(action: Action<KobolDependencies>) {
        action.execute(dependencies)
    }
}
