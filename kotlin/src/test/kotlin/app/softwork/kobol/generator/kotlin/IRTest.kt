package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.ir.KobolIRTree.Types.Function.Statement.*
import kotlin.test.*

class IRTest {
    @Test
    fun tryCatch() {
        val Exception = KobolIRTree.Types.Type.Class(
            "Exception",
            packageName = "kotlin",
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
        // language=kotlin
        val expected = """
            package foo
            
            import kotlin.Exception
            
            public fun foo() {
              try {
                println("TRY I")
                println("TRY II")
              } catch(exception: Exception) {
                println("CATCH I")
                println("CATCH II")
              } catch(exception: Exception) {
                println("CATCH III")
                println("CATCH IV")
              } finally {
                println("FINALLY I")
                println("FINALLY II")
              }
            
            }
            
        """.trimIndent()

        assertEquals(expected, generate(ir).toString())
    }
}
