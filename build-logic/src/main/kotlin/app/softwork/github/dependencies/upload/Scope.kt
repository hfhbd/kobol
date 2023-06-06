package app.softwork.github.dependencies.upload

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
enum class Scope: java.io.Serializable {
    @SerialName("runtime")
    Runtime,

    @SerialName("development")
    Development
}
