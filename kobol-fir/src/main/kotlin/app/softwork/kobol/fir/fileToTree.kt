package app.softwork.kobol.fir

import app.softwork.kobol.*
import app.softwork.sqldelight.db2dialect.grammar.*
import com.alecstrong.sql.psi.core.*
import com.intellij.lang.*
import com.intellij.openapi.project.*
import com.intellij.psi.*
import com.intellij.psi.stubs.*
import com.intellij.psi.tree.*
import java.io.*

public fun File.toTree(firPlugins: List<FirPlugin> = emptyList()): CobolFIRTree {
    return toCobolFile().toTree().let {
        var it = it
        for (plugin in firPlugins) {
            it = plugin.invoke(it)
        }
        it
    }
}

public fun File.toCobolFile(): CobolFile {
    val intelliJ = CoreEnvironment(listOf(this)).apply {
        initializeApplication {
            registerFileType(CobolFileType.INSTANCE, CobolFileType.INSTANCE.defaultExtension)
            registerParserDefinition(CobolParserDefinition())

            registerFileType(InlineSqlFileType, InlineSqlFileType.defaultExtension)
            registerParserDefinition(Db2ParserDefinition())
        }
    }
    lateinit var file: CobolFile
    intelliJ.forSourceFile<CobolFile> {
        file = it
    }
    return file
}

public fun Iterable<File>.toTree(firPlugins: List<FirPlugin> = emptyList()): List<CobolFIRTree> {
    return toCobolFile().map {
        try {
            it.toTree().let {
                var it = it
                for (plugin in firPlugins) {
                    it = plugin.invoke(it)
                }
                it
            }
        } catch (e: Exception) {
            throw IllegalStateException(it.virtualFile.presentableUrl, e).apply {
                stackTrace = e.stackTrace
            }
        }
    }
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

public fun Iterable<File>.toCobolFile(): Set<CobolFile> {
    val intelliJ = CoreEnvironment(this).apply {
        initializeApplication {
            registerFileType(CobolFileType.INSTANCE, CobolFileType.INSTANCE.defaultExtension)
            registerParserDefinition(CobolParserDefinition())

            registerFileType(InlineSqlFileType, InlineSqlFileType.defaultExtension)
            registerParserDefinition(Db2ParserDefinition())
        }
    }
    return buildSet {
        intelliJ.forSourceFiles(CobolFile::class.java) {
            add(it)
        }
    }
}