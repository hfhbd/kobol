package app.softwork.kobol.generator.java

import app.softwork.kobol.ir.CodeGeneratorFactory
import app.softwork.serviceloader.ServiceLoader
import java.io.File

@ServiceLoader(CodeGeneratorFactory::class)
public class JavaCodeGeneratorFactory : CodeGeneratorFactory {
    override operator fun invoke(outputFolder: File, args: Map<String, String>): JavaCodeGenerator =
        JavaCodeGenerator(File(outputFolder, "java"))
}
