package app.softwork.github.dependencies.upload

import kotlinx.serialization.Serializable

@Serializable
data class Detector(
    val name: String,
    val version: String,
    val url: String
)
