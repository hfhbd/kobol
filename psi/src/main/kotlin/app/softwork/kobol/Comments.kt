package app.softwork.kobol

import com.intellij.psi.tree.*

private val commentTokens = TokenSet.create(CobolTypes.COMMENT)

public fun CobolComments.asComments(): List<String> = node.getChildren(commentTokens).map {
    it.text.drop(1).trim()
}
