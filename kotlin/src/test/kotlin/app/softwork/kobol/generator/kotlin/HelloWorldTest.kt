package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.fir.*
import app.softwork.kobol.ir.*
import app.softwork.kobol.plugins.ir.*
import app.softwork.kobol.plugins.ir.optimizations.*
import java.io.*
import java.nio.file.*
import kotlin.test.*

class HelloWorldTest {
    @Test
    fun helloWorld() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WO-RLD PIC X(6) VALUE 'WORLD!'.
        123456 PROCEDURE                   DIVISION.
        123456
        123456* Some Comment
        123456     DISPLAY "HELLO " WO-RLD
        123456     MOVE "42" TO WO-RLD
        123456     DISPLAY WO-RLD
        123456     DISPLAY "ANSWER"WO-RLD.
        """.trimIndent().toIR(packageName = "foo") { _, _ ->
            "main"
        }

        val output = generate(input)

        //language=kotlin
        val expected = """
        package foo.hello
        
        import kotlin.String
        
        public var `WO-RLD`: String = "WORLD!"
        
        public fun main() {
          // Some Comment
          println("HELLO ${'$'}`WO-RLD`")
          `WO-RLD` = "42"
          println(`WO-RLD`)
          println("ANSWER${'$'}`WO-RLD`")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun mainTest() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 LINKAGE SECTION.
        123456 77 WO-RLD PIC X(6) VALUE 'WORLD!'.
        123456 PROCEDURE DIVISION USING WO-RLD.
        123456
        123456* Some Comment
        123456     DISPLAY "HELLO " WO-RLD
        123456     MOVE "42" TO WO-RLD
        123456     DISPLAY WO-RLD
        123456     DISPLAY "ANSWER"WO-RLD.
        """.trimIndent().toIR(
            packageName = "foo",
            irPlugins = listOf(
                NoSynthetics(),
                IrPlugin { tree, others ->
                    others + tree.addMainEntrypoint { main, args ->
                        val singleFun = function("single") {}
                        +main(args.use(singleFun()))
                    }
                },
            ),
        )

        val output = generate(input)

        //language=kotlin
        val expected = """
        package foo.hello
        
        import kotlin.Array
        import kotlin.String
        
        public fun main(args: Array<String>) {
          hello(args.single())
        }
        
        public fun hello(`WO-RLD`: String) {
          // Some Comment
          println("HELLO ${'$'}`WO-RLD`")
          `WO-RLD` = "42"
          println(`WO-RLD`)
          println("ANSWER${'$'}`WO-RLD`")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}

internal fun String.toIR(
    vararg including: Triple<String?, String, String>,
    firPlugins: List<FirPlugin> = emptyList(),
    irPlugins: List<IrPlugin> = listOf(NoSynthetics()),
    fileConverter: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String, File) -> SqlPrecompiler)? = null,
    controlFlowHandling: ((String) -> ControlFlowHandling)? = null,
    packageName: String? = null,
    procedureName: ProcedureName? = null,
): KobolIRTree {
    val tempPath = Files.createTempDirectory("testing")
    val temp = tempPath.toFile()
    val files = including.map { (packageName, name, content) ->
        val packageFile = if (packageName != null) {
            val fileName = temp.absolutePath + "/" + packageName
            File(fileName).apply { mkdirs() }
        } else {
            temp
        }

        File(packageFile, name).apply { writeText(content) }
    }
    val packageFile = if (packageName != null) {
        val fileName = temp.absolutePath + "/" + packageName
        File(fileName).apply { mkdirs() }
    } else {
        temp
    }
    return (files + File(packageFile, "testing.cbl").apply { writeText(this@toIR) }).toIR(
        firPlugins = firPlugins,
        irPlugins = irPlugins,
        fileConverter = fileConverter,
        serialization = serialization,
        sqlPrecompiler = sqlPrecompiler?.let { getSQL ->
            {
                getSQL(it, temp)
            }
        },
        controlFlowHandling = controlFlowHandling,
        procedureName = procedureName,
        absoluteBasePath = tempPath,
    ).single()
}
