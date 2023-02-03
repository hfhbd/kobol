package app.softwork.kobol.generator.kotlin.optimization

import app.softwork.kobol.generator.*
import app.softwork.kobol.generator.kotlin.*
import app.softwork.kobol.generator.kotlin.toIR
import app.softwork.kobol.ir.*
import app.softwork.kobol.plugins.fir.renaming.*
import app.softwork.kobol.plugins.ir.optimizations.*
import org.intellij.lang.annotations.*
import kotlin.test.*

class OptimizationTest {
    @Test
    fun readonlyVariables() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456 PROCEDURE                   DIVISION.
        123456     DISPLAY HELLO WORLD
        123456* Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR()
            .let { ReadOnlyVariables()(it, emptyList()) }.single()

        val output = generate(input)

        @Language("kotlin")
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public var WORLD: String = "WORLD!"
        
        public val HELLO: String = "HELLO"
        
        public fun main(): Unit {
          println("${'$'}HELLO${'$'}WORLD")
          // Some Comment
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun readonlyVariablesConst() {
        //language=cobol
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456 PROCEDURE                   DIVISION.
        123456     DISPLAY HELLO WORLD
        123456* Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR()
            .let { ReadOnlyVariables()(it, emptyList()) }.single()
            .let { ConstVariables()(it, emptyList()) }.single()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public var WORLD: String = "WORLD!"
        
        public const val HELLO: String = "HELLO"
        
        public fun main(): Unit {
          println("${'$'}HELLO${'$'}WORLD")
          // Some Comment
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun private() {
        //language=COBOL
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456 PROCEDURE                   DIVISION.
        123456     DISPLAY HELLO WORLD
        123456* Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR()
            .let { Private()(it, emptyList()) }.single()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        private var WORLD: String = "WORLD!"
        
        private var HELLO: String = "HELLO"
        
        public fun main(): Unit {
          println("${'$'}HELLO${'$'}WORLD")
          // Some Comment
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun javaNames() {
        //language=COBOL
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456 PROCEDURE                   DIVISION.
        123456     DISPLAY HELLO WORLD.
        123456
        123456 FOO-BAR SECTION.
        123456* Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR(firPlugins = listOf(JavaNames()))

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public fun fooBar(): Unit {
          // Some Comment
          world = "42"
          println("ANSWER${'$'}world")
        }
        
        public var world: String = "WORLD!"
        
        public var hello: String = "HELLO"
        
        public fun main(): Unit {
          println("${'$'}hello${'$'}world")
          fooBar()
        }

        """.trimIndent()
        assertEquals(expected, output.toString())
    }
    
    @Test
    fun keepNames() {
        //language=COBOL
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456 PROCEDURE                   DIVISION.
        123456     DISPLAY HELLO WORLD.
        123456
        123456 FOO-BAR SECTION.
        123456* Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR(firPlugins = listOf(KeepNames()))

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public fun FOO_BAR(): Unit {
          // Some Comment
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        public var WORLD: String = "WORLD!"
        
        public var HELLO: String = "HELLO"
        
        public fun main(): Unit {
          println("${'$'}HELLO${'$'}WORLD")
          FOO_BAR()
        }

        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun optimize() {
        //language=COBOL
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 FOO PIC 9 VALUE 1.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456 PROCEDURE                   DIVISION.
        123456     DISPLAY HELLO WORLD.
        123456
        123456 FOO SECTION.
        123456* Some Comment
        123456     MOVE "42" TO WORLD
        123456     PERFORM VARYING FOO FROM 1 BY 1 UNTIL WORLD = "42"
        123456         DISPLAY FOO
        123456     END-PERFORM
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR(firPlugins = listOf(JavaNames()))
            .let { Optimize(it, emptyList()) }.single()

        val output = generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.Int
        import kotlin.String
        import kotlin.Unit
        
        private fun foo(): Unit {
          // Some Comment
          world = "42"
          foo = 1
          while (world != "42") {
            println(foo)
            foo += 1
          }
          println("ANSWER${'$'}world")
        }
        
        private var foo: Int = 1
        
        private var world: String = "WORLD!"
        
        private const val hello: String = "HELLO"
        
        public fun main(): Unit {
          println("${'$'}hello${'$'}world")
          foo()
        }

        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
