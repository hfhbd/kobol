package app.softwork.kobol.generator.kotlin

import app.softwork.kobol.fir.*
import app.softwork.kobol.ir.*
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
        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public var `WO-RLD`: String = "WORLD!"
        
        public fun main(): Unit {
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
    fun jniMain() {
        //language=cobol
        val input = """
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. MAIN.
000021 DATA DIVISION.
000022 WORKING-STORAGE SECTION.
000023 01 AO.
000023* FFF
000024  05 FOO PIC X(12) VALUE 'RSTXYZRSTXYZ'.
000025  05 B PIC 9 VALUE 5.
000030 PROCEDURE DIVISION.
000040     DISPLAY "MAIN " FOO
000050     CALL 'HELLO' USING AO.

        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
package main

import kotlin.Int
import kotlin.String
import kotlin.Unit

public object AO {
  /**
   * FFF
   */
  public var FOO: String = "RSTXYZRSTXYZ"

  public var B: Int = 5
}

public object HELLO {
  init {
    System.loadLibrary("hello")
  }

  public external operator fun invoke(FOO: String, B: Int): Unit
}

public fun main(): Unit {
  println("MAIN ${'$'}{AO.FOO}")
  HELLO(AO.FOO, AO.B)
}

        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun jniHello() {
        //language=cobol
        val input = """
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. HELLO.
000030 DATA DIVISION.
111111 WORKING-STORAGE SECTION.
000000 77 W1-EBCDIC    PIC X(12).
000000 77 W1-CCSID     PIC X(4) VALUE '1140'.
000000 77 W2-UTF-8     PIC X(12).
000000 77 W2-CCSID     PIC X(4) VALUE '1208'.
000040 LINKAGE SECTION.
000041 01 BB.
000042   05 BAR PIC X(6).
000042   05 BAR2 PIC X(6).
000043* 77 AB PIC S9(9) USAGE IS BINARY.
000050 PROCEDURE DIVISION USING BB.
00000      MOVE BB TO W2-UTF-8
000000*    MOVE FUNCTION DISPLAY-OF (FUNCTION NATIONAL-OF
000000*          (W2-UTF-8, 1208), 1140) TO W1-EBCDIC
000000
000000     DISPLAY W1-EBCDIC
000000
000060     DISPLAY "HELLO"
234234     DISPLAY "InOrder: " BAR BAR2.

        """.trimIndent().toIR()

        val output = generate(input)

        //language=kotlin
        val expected = """
package main

import kotlin.Int
import kotlin.String
import kotlin.Unit

public object AO {
  /**
   * FFF
   */
  public var FOO: String = "RSTXYZRSTXYZ"

  public var B: Int = 5
}

public object HELLO {
  init {
    System.loadLibrary("hello")
  }

  public external operator fun invoke(FOO: String, B: Int): Unit
}

public fun main(): Unit {
  println("MAIN ${'$'}{AO.FOO}")
  HELLO(AO.FOO, AO.B)
}

        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}

internal fun String.toIR(
    vararg including: Pair<String, String>,
    firPlugins: List<FirPlugin> = emptyList(),
    irPlugins: List<IrPlugin> = listOf(NoSynthetics()),
    fileConverter: ((String) -> FileHandling)? = null,
    serialization: ((String) -> SerializationPlugin)? = null,
    sqlPrecompiler: ((String, File) -> SqlPrecompiler)? = null,
    controlFlowHandling: ((String) -> ControlFlowHandling)? = null,
): KobolIRTree {
    val temp = Files.createTempDirectory("testing").toFile()
    val files = including.map { (name, content) ->
        File(temp, name).apply { writeText(content) }
    }
    return (files + File(temp, "testing.cbl").apply { writeText(this@toIR) }).toIR(
        firPlugins = firPlugins,
        irPlugins = irPlugins,
        fileConverter = fileConverter,
        serialization = serialization,
        sqlPrecompiler = sqlPrecompiler?.let { getSQL ->
            {
                getSQL(it, temp)
            }
        },
        controlFlowHandling = controlFlowHandling
    ).single()
}
