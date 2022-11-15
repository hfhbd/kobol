package app.softwork.kobol

import com.intellij.psi.tree.*

internal class CobolTokenType(debugName: String) : IElementType(debugName, CobolLanguage) {
    override fun toString(): String = "CobolTokenType." + super.toString()
}
