package app.softwork.kobol.gradle

import app.softwork.kobol.fir.*
import app.softwork.kobol.ir.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.workers.*
import java.io.*
import java.util.*

public abstract class ExecuteKobol : WorkAction<ExecuteKobol.Parameters> {
    public interface Parameters : WorkParameters {
        public val inputFiles: ConfigurableFileCollection
        public val outputFolder: DirectoryProperty
        public val sqlFolder: DirectoryProperty
        public val config: MapProperty<String, Map<String, String>>
    }

    internal companion object {
        operator fun <T : CodeGenerator> invoke(
            input: Set<File>,
            outputFolder: File,
            sqlFolder: File? = null,
            config: Map<String, Map<String, String>> = emptyMap(),
            firPlugins: Iterable<FirPlugin> = emptyList(),
            irPlugins: Iterable<IrPlugin> = emptyList(),
            sql: SqlPrecompilerFactory? = null,
            files: FileHandlingFactory? = null,
            serialization: SerializationPluginFactory? = null,
            codeGeneratorFactory: CodeGeneratorFactory<T>
        ) {
            val codeGeneratorConfig = config[CodeGenerator::class.qualifiedName!!] ?: emptyMap()
            val codeGenerator = codeGeneratorFactory(outputFolder, codeGeneratorConfig)

            val irs = input.toIR(
                firPlugins = firPlugins,
                sqlPrecompiler = sql?.let {
                    {
                        sql(
                            packageName = it,
                            fileName = it,
                            outputFolder = sqlFolder,
                            args = config[SqlPrecompiler::class.qualifiedName!!] ?: emptyMap()
                        )
                    }
                },
                fileConverter = files?.let {
                    {
                        files(it, config[FileHandling::class.qualifiedName!!] ?: emptyMap())
                    }
                },
                serialization = serialization?.let {
                    {
                        serialization(it, config[SerializationPlugin::class.qualifiedName!!] ?: emptyMap())
                    }
                },
                irPlugins = irPlugins
            )

            codeGenerator.generate(irs)
            codeGenerator.close()
        }
    }

    override fun execute() {
        val codeGenerators = ServiceLoader.load(CodeGeneratorFactory::class.java)
        val firPlugins = ServiceLoader.load(FirPluginBeforePhase::class.java) + ServiceLoader.load(
            FirPluginAfterPhase::class.java
        )
        val irPlugins = ServiceLoader.load(IrPlugin::class.java)
        val sql = ServiceLoader.load(SqlPrecompilerFactory::class.java).singleOrNull()
        val files = ServiceLoader.load(FileHandlingFactory::class.java).singleOrNull()
        val serialization = ServiceLoader.load(SerializationPluginFactory::class.java).singleOrNull()
        
        for (codeGenerator in codeGenerators) {
            invoke(
                input = parameters.inputFiles.files,
                outputFolder = parameters.outputFolder.get().asFile,
                sqlFolder = parameters.sqlFolder.asFile.orNull,
                config = parameters.config.get(),
                firPlugins = firPlugins,
                irPlugins = irPlugins,
                sql = sql,
                files = files,
                serialization = serialization,
                codeGeneratorFactory = codeGenerator
            )
        }
    }
}
