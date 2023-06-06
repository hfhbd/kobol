package app.softwork.github.dependencies.upload

import kotlinx.datetime.Instant
import kotlinx.serialization.Serializable

// https://docs.github.com/en/rest/dependency-graph/dependency-submission?apiVersion=2022-11-28
@Serializable
data class Upload(
    val version: Int,
    val job: Job,
    val sha: String,
    val ref: String,
    val detector: Detector,
    val metadata: Map<String, String>? = null,
    val manifests: Map<String, Manifest>,
    val scanned: Instant
)
