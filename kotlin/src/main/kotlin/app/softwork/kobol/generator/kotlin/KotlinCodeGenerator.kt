package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.ir.CodeGenerator
import app.softwork.kobol.ir.CodeGeneratorFactory
import app.softwork.kobol.ir.KobolIRTree
import app.softwork.serviceloader.ServiceLoader
import java.io.File

public class KotlinCodeGenerator(private val output: File) : CodeGenerator {
    override fun generate(ir: Iterable<KobolIRTree>) {
        for (tree in ir) {
            val kotlin = generate(tree)
            kotlin.writeTo(directory = output)
        }
    }
}

@ServiceLoader(CodeGeneratorFactory::class)
public class KotlinCodeGeneratorFactory : CodeGeneratorFactory {
    override operator fun invoke(outputFolder: File, args: Map<String, String>): KotlinCodeGenerator {
        return KotlinCodeGenerator(File(outputFolder, "kotlin"))
    }
}
