package app.softwork.kobol

import app.softwork.kobol.fir.*
import app.softwork.sqldelight.db2dialect.grammar.*
import com.alecstrong.sql.psi.core.*
import com.intellij.lang.*
import com.intellij.openapi.project.*
import com.intellij.psi.*
import com.intellij.psi.stubs.*
import com.intellij.psi.tree.*
import java.io.*
import java.nio.file.Path

public fun File.toTree(
    absoluteBasePath: Path,
    firPlugins: List<FirPlugin> = emptyList(),
): CobolFIRTree =
    setOf(this).toTree(absoluteBasePath, firPlugins).single()

public fun Iterable<File>.toTree(
    absoluteBasePath: Path,
    firPlugins: Iterable<FirPlugin> = emptyList(),
): Iterable<CobolFIRTree> {
    val beforePhases = mutableListOf<FirPluginBeforePhase>()
    val afterPhases = mutableListOf<FirPluginAfterPhase>()

    for (firPlugin in firPlugins) {
        when (firPlugin) {
            is FirPluginBeforePhase -> beforePhases.add(firPlugin)
            is FirPluginAfterPhase -> afterPhases.add(firPlugin)
        }
    }

    var cobolTrees = buildMap {
        for (file in toCobolFile()) {
            try {
                var tree = file.toTree(absoluteBasePath)
                for (plugin in beforePhases) {
                    tree = plugin(tree)
                }
                this[tree.fileName] = tree
            } catch (ignored: Exception) {
                throw IllegalStateException(file.virtualFile.name, ignored).apply {
                    stackTrace = ignored.stackTrace
                }
            }
        }
    }
    for (beforePhase in beforePhases) {
        beforePhase.close()
    }

    for (afterPhase in afterPhases) {
        for (tree in cobolTrees) {
            val other = cobolTrees - tree.key
            val newTrees = afterPhase(tree.value, other.values).associateBy { it.fileName }
            cobolTrees = newTrees
        }
    }

    for (afterPhase in afterPhases) {
        afterPhase.close()
    }

    return cobolTrees.values
}

private class Db2ParserDefinition : SqlParserDefinition() {
    override fun createFile(viewProvider: FileViewProvider) = InlineSqlFile(viewProvider)
    override fun createParser(project: Project): SqlParser {
        SqlParserUtil.reset()

        SqlParserUtil.reset()
        Db2ParserUtil.reset()
        Db2ParserUtil.overrideSqlParser()

        return SqlParser()
    }

    override fun getFileNodeType(): IFileElementType = FILE

    override fun getLanguage(): Language = SqlInlineLanguage

    companion object {
        val FILE = ILightStubFileElementType<PsiFileStub<InlineSqlFile>>(SqlInlineLanguage)
    }
}

public fun File.toCobolFile(): CobolFile = setOf(this).toCobolFile().single()

public fun Iterable<File>.toCobolFile(): Collection<CobolFile> {
    System.setProperty("java.awt.headless", "true")

    val intelliJ = object : SqlCoreEnvironment(
        sourceFolders = toList(),
        dependencies = emptyList(),
    ) {
        init {
            initializeApplication {
                registerFileType(CobolFileType.INSTANCE, CobolFileType.INSTANCE.defaultExtension)
                registerParserDefinition(CobolParserDefinition())

                registerFileType(InlineSqlFileType, InlineSqlFileType.defaultExtension)
                registerParserDefinition(Db2ParserDefinition())
            }
        }
    }
    return buildSet {
        intelliJ.forSourceFiles<CobolFile> {
            add(it)
        }
    }
}
