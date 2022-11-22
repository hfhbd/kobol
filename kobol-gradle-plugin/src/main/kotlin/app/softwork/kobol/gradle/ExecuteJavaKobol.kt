package app.softwork.kobol.gradle

import app.softwork.kobol.fir.*
import app.softwork.kobol.generator.java.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.optimizations.*
import org.gradle.api.file.*
import org.gradle.api.provider.*
import org.gradle.workers.*
import java.io.*
import java.util.*

public abstract class ExecuteJavaKobol: WorkAction<ExecuteJavaKobol.Parameters> {
    public interface Parameters: WorkParameters {
        public val inputFiles: ConfigurableFileCollection
        public val outputFolder: DirectoryProperty
        public val sqlFolder: DirectoryProperty
        public val optimize: Property<Boolean>
        public val config: MapProperty<String, Map<String, String>>
        public val java8: Property<Boolean>
    }

    override fun execute() {
        val input: Set<File> = parameters.inputFiles.files
        val outputFolder = parameters.outputFolder.get().asFile

        val firPlugins = ServiceLoader.load(FirPluginBeforePhase::class.java) + ServiceLoader.load(FirPluginAfterPhase::class.java)
        val irPlugins = ServiceLoader.load(IrPlugin::class.java).toMutableList()

        if (parameters.optimize.get()) {
            irPlugins += Optimize
        }

        val sqlFolder = parameters.sqlFolder.asFile.orNull
        val sql = ServiceLoader.load(SqlPrecompilerFactory::class.java).singleOrNull()
        val files = ServiceLoader.load(FileHandlingFactory::class.java).singleOrNull()
        val serialization = ServiceLoader.load(SerializationPluginFactory::class.java).singleOrNull()

        val config: Map<String, Map<String, String>> = parameters.config.get()

        generate(
            firPlugins = firPlugins,
            files = input,
            output = outputFolder,
            java8 = parameters.java8.get(),
            sqlPrecompiler = sql?.let {
                {
                    sql(
                        packageName = it,
                        fileName = it,
                        outputFolder = sqlFolder,
                        args = config[SqlPrecompiler::class.qualifiedName] ?: emptyMap()
                    )
                }
            },
            fileHandling = files?.let {
                {
                    files(it, config[FileHandling::class.qualifiedName] ?: emptyMap())
                }
            },
            serialization = serialization?.let {
                {
                    serialization(it, config[SerializationPlugin::class.qualifiedName] ?: emptyMap())
                }
            },
            irPlugins = irPlugins
        )
    }
}
