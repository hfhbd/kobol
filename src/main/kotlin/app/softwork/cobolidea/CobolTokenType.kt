package app.softwork.cobolidea

import com.intellij.psi.tree.*

class CobolTokenType(
    debugName: String
) : IElementType(
    debugName, CobolLanguage
) {
    override fun toString() = "CobolTokenType." + super.toString()
}
