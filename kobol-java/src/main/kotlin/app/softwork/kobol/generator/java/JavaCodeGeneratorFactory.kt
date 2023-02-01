package app.softwork.kobol.generator.java

import app.softwork.kobol.ir.*
import java.io.*

public class JavaCodeGeneratorFactory : CodeGeneratorFactory {
    override operator fun invoke(outputFolder: File, args: Map<String, String>): JavaCodeGenerator =
        JavaCodeGenerator(File(outputFolder, "java"))
}
