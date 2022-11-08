package app.softwork.kobol

import com.intellij.openapi.fileTypes.*
import com.intellij.openapi.project.*
import com.intellij.openapi.vfs.*

class CobolSyntaxHighlighterFactory : SyntaxHighlighterFactory() {
    override fun getSyntaxHighlighter(project: Project?, virtualFile: VirtualFile?) =
        CobolSyntaxHighlighter
}
