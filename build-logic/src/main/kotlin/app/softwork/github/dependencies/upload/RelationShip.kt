package app.softwork.github.dependencies.upload

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
enum class RelationShip: java.io.Serializable {
    @SerialName("direct")
    Direct,

    @SerialName("indirect")
    Indirect
}
