package app.softwork.kobol.generator.java

import app.softwork.kobol.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import kotlin.test.*

class IRTestJava {
    @Test
    fun tryCatch() {
        val Exception = KobolIRTree.Types.Type.Class(
            "Exception",
            packageName = "java.lang",
        )
        val ir = KobolIRTree(
            id = "foo",
            name = "foo",
            main = function("foo") {
                +TryCatch(
                    tryStmts = build {
                        +Print("TRY I".l)
                        +Print("TRY II".l)
                    },
                    catchBlocks = build {
                        +TryCatch.CatchBlock(
                            exceptionClass = Exception,
                            stmts = build {
                                +Print("CATCH I".l)
                                +Print("CATCH II".l)
                            },
                        )
                        +TryCatch.CatchBlock(
                            exceptionClass = Exception,
                            stmts = build {
                                +Print("CATCH III".l)
                                +Print("CATCH IV".l)
                            },
                        )
                    },
                    finallyStmts = build {
                        +Print("FINALLY I".l)
                        +Print("FINALLY II".l)
                    },
                )
            },
            types = emptyList(),
        )
        // language=Java
        val expected = """
            package foo;
            
            public class Foo {
              public static void foo() {
                try {
                  System.out.println("TRY I");
                  System.out.println("TRY II");
                } catch (Exception exception) {
                  System.out.println("CATCH I");
                  System.out.println("CATCH II");
                } catch (Exception exception) {
                  System.out.println("CATCH III");
                  System.out.println("CATCH IV");
                } finally {
                  System.out.println("FINALLY I");
                  System.out.println("FINALLY II");
                }
              }
            }
            
        """.trimIndent()

        assertEquals(expected, generateJava(ir).single().toString())
    }
}
