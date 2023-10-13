package app.softwork.kobol.gradle

import org.gradle.api.provider.*
import org.gradle.workers.*

internal interface SshParameters : WorkParameters {
    val host: Property<String>
    val user: Property<String>
    val folder: Property<String>
}
