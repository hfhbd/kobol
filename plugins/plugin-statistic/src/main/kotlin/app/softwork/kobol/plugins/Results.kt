package app.softwork.kobol.plugins

import kotlinx.serialization.*

@Serializable
public data class Results(val complexity: Map<String, Int>)
