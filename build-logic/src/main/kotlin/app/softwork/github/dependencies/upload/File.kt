package app.softwork.github.dependencies.upload

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class File(
    @SerialName("source_location")
    val sourceLocation: String?
)
