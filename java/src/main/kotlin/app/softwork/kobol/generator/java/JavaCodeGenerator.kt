package app.softwork.kobol.generator.java

import app.softwork.kobol.ir.*
import java.io.*

public class JavaCodeGenerator(private val output: File) : CodeGenerator {
    override fun generate(ir: Iterable<KobolIRTree>) {
        for (tree in ir) {
            for (javaFile in generateJava(tree)) {
                javaFile.writeTo(output)
            }
        }
    }
}
