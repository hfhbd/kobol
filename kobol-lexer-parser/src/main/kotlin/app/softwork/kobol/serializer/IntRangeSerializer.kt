package app.softwork.kobol.serializer

import kotlinx.serialization.*
import kotlinx.serialization.encoding.*

@Serializable
private data class IntRangeData(val start: Int, val endInclusive: Int) {
    fun toRange() = IntRange(start, endInclusive)
}

object IntRangeSerializer : KSerializer<IntRange> {
    override val descriptor = IntRangeData.serializer().descriptor

    override fun deserialize(decoder: Decoder): IntRange =
        decoder.decodeSerializableValue(IntRangeData.serializer()).toRange()

    override fun serialize(encoder: Encoder, value: IntRange) {
        encoder.encodeSerializableValue(
            IntRangeData.serializer(),
            IntRangeData(start = value.first, endInclusive = value.last)
        )
    }
}
