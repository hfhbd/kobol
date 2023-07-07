package app.softwork.kobol.gradle

import app.softwork.kobol.fir.*
import app.softwork.kobol.ir.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.workers.*
import java.io.*
import java.util.*

internal abstract class ExecuteKobol : WorkAction<ExecuteKobol.Parameters> {
    internal interface Parameters : WorkParameters {
        val inputFiles: ConfigurableFileCollection
        val outputFolder: DirectoryProperty
        val sqlFolder: DirectoryProperty
        val config: MapProperty<String, Map<String, String>>
    }

    internal companion object {
        operator fun invoke(
            input: Set<File>,
            outputFolder: File,
            sqlFolder: File? = null,
            config: Map<String, Map<String, String>> = emptyMap(),
            firPlugins: Iterable<FirPlugin> = emptyList(),
            irPlugins: Iterable<IrPlugin> = emptyList(),
            sql: SqlPrecompilerFactory? = null,
            files: FileHandlingFactory? = null,
            serialization: SerializationPluginFactory? = null,
            controlFlowHandling: ControlFlowHandlingFactory? = null,
            codeGeneratorFactory: CodeGeneratorFactory
        ) {
            val codeGeneratorConfig = config[CodeGenerator::class.qualifiedName!!] ?: emptyMap()
            val codeGenerator = codeGeneratorFactory(outputFolder, codeGeneratorConfig)

            val closeables = mutableListOf<AutoCloseable>(codeGenerator)
            val irs = input.toIR(
                firPlugins = firPlugins,
                sqlPrecompiler = sql?.let {
                    {
                        sql(
                            packageName = it,
                            fileName = it,
                            outputFolder = sqlFolder,
                            args = config[SqlPrecompiler::class.qualifiedName!!] ?: emptyMap()
                        ).also { closeables.add(it) }
                    }
                },
                fileConverter = files?.let {
                    {
                        files(it, config[FileHandling::class.qualifiedName!!] ?: emptyMap()).also { closeables.add(it) }
                    }
                },
                serialization = serialization?.let {
                    {
                        serialization(
                            it,
                            config[SerializationPlugin::class.qualifiedName!!] ?: emptyMap()
                        ).also { closeables.add(it) }
                    }
                },
                controlFlowHandling = controlFlowHandling?.let {
                    {
                        controlFlowHandling()
                    }
                },
                irPlugins = irPlugins
            )

            codeGenerator.generate(irs)
            for (toClose in closeables) {
                toClose.close()
            }
        }
    }

    override fun execute() {
        val codeGenerators = ServiceLoader.load(CodeGeneratorFactory::class.java).toList()
        val firPlugins = ServiceLoader.load(FirPluginBeforePhase::class.java) + ServiceLoader.load(
            FirPluginAfterPhase::class.java
        )
        val irPlugins = ServiceLoader.load(IrPlugin::class.java).toList()
        val sql = ServiceLoader.load(SqlPrecompilerFactory::class.java).singleOrNull()
        val files = ServiceLoader.load(FileHandlingFactory::class.java).singleOrNull()
        val serialization = ServiceLoader.load(SerializationPluginFactory::class.java).singleOrNull()
        val controlFlowHandlingFactory = ServiceLoader.load(ControlFlowHandlingFactory::class.java).singleOrNull()

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
                codeGeneratorFactory = codeGenerator,
                controlFlowHandling = controlFlowHandlingFactory
            )
        }
        firPlugins.forEach(AutoCloseable::close)
        irPlugins.forEach(AutoCloseable::close)
    }
}
