package app.softwork.kobol.fir.serializer

import kotlinx.serialization.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.*

internal object NothingSerializer : KSerializer<Nothing> {
    override val descriptor = buildClassSerialDescriptor("kotlin.Nothing")

    override fun deserialize(decoder: Decoder): Nothing =
        throw UnsupportedOperationException("Nothing does not have instances")

    override fun serialize(encoder: Encoder, value: Nothing): Nothing =
        throw UnsupportedOperationException("Nothing cannot be serialized")
}
