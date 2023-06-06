package app.softwork.github.dependencies.upload

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class Resolved(
    @SerialName("package_url")
    val packageURL: String?,
    val metadata: Map<String, String>? = null,
    val relationShip: RelationShip?,
    val scope: Scope?,
    val dependencies: List<String>?
): java.io.Serializable
