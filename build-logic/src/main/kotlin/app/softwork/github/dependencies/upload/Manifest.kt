package app.softwork.github.dependencies.upload

import kotlinx.serialization.Serializable

@Serializable
data class Manifest(
    val name: String,
    val file: File?,
    val metadata: Map<String, String>? = null,
    val resolved: Map<String, Resolved>?
)
