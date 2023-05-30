package app.softwork.github.dependencies.upload

import kotlinx.serialization.Serializable

@Serializable
data class Job(
    val id: String,
    val correlator: String,
    val html_url: String?
)
