package app.softwork.kobol.intellij

import app.softwork.kobol.CobolFile
import app.softwork.kobol.CobolProcedureSection
import com.intellij.lang.ASTNode
import com.intellij.lang.folding.FoldingBuilderEx
import com.intellij.lang.folding.FoldingDescriptor
import com.intellij.openapi.editor.Document
import com.intellij.openapi.project.DumbAware
import com.intellij.psi.PsiElement
import com.intellij.util.containers.toArray

class CobolFoldingBuilder : FoldingBuilderEx(), DumbAware {
    override fun buildFoldRegions(
        root: PsiElement,
        document: Document,
        quick: Boolean,
    ): Array<FoldingDescriptor> = (root as CobolFile).programOrNull?.procedureDiv?.procedureSectionList?.map {
        FoldingDescriptor(
            it.node,
            it.textRange,
        )
    }?.toArray(FoldingDescriptor.EMPTY_ARRAY) ?: FoldingDescriptor.EMPTY_ARRAY

    override fun isCollapsedByDefault(node: ASTNode): Boolean = false

    override fun getPlaceholderText(node: ASTNode): String? =
        (node.psi as? CobolProcedureSection)?.sectionID?.text?.let { "$it SECTION." }
}
