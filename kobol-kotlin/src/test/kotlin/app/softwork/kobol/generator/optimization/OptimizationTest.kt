package app.softwork.kobol.generator.optimization

import app.softwork.kobol.generator.*
import app.softwork.kobol.optimizations.*
import kotlin.test.*

class OptimizationTest {
    @Test
    fun readonlyVariables() {
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456******************************************************************
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456******************************************************************
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456/*****************************************************************
        123456 PROCEDURE                   DIVISION.
        123456******************************************************************
        123456     DISPLAY HELLO WORLD
        123456 * Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR()
            .readonlyVariables()

        val output = KotlinGenerator.generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public var WORLD: String = "WORLD!"

        public val HELLO: String = "HELLO"
        
        public fun main(): Unit {
          println("${'$'}HELLO${'$'}WORLD")
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun readonlyVariablesConst() {
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456******************************************************************
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456******************************************************************
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456/*****************************************************************
        123456 PROCEDURE                   DIVISION.
        123456******************************************************************
        123456     DISPLAY HELLO WORLD
        123456 * Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR()
            .readonlyVariables()
            .constVariables()

        val output = KotlinGenerator.generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public var WORLD: String = "WORLD!"

        public const val HELLO: String = "HELLO"
        
        public fun main(): Unit {
          println("${'$'}HELLO${'$'}WORLD")
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun private() {
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456******************************************************************
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456******************************************************************
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456/*****************************************************************
        123456 PROCEDURE                   DIVISION.
        123456******************************************************************
        123456     DISPLAY HELLO WORLD
        123456 * Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR()
            .private()

        val output = KotlinGenerator.generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        private var WORLD: String = "WORLD!"

        private var HELLO: String = "HELLO"
        
        public fun main(): Unit {
          println("${'$'}HELLO${'$'}WORLD")
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun camelCase() {
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456******************************************************************
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456******************************************************************
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456/*****************************************************************
        123456 PROCEDURE                   DIVISION.
        123456******************************************************************
        123456     DISPLAY HELLO WORLD
        123456     PERFORM FOO.
        123456
        123456 FOO SECTION.
        123456 * Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR()
            .camelCase()

        val output = KotlinGenerator.generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public fun foo(): Unit {
          world = "42"
          println("ANSWER${'$'}world")
        }

        public var world: String = "WORLD!"

        public var hello: String = "HELLO"

        public fun main(): Unit {
          println("${'$'}hello${'$'}world")
          foo()
        }

        """.trimIndent()
        assertEquals(expected, output.toString())
    }

    @Test
    fun optimize() {
        val input = """
        123456 IDENTIFICATION              DIVISION.
        123456******************************************************************
        123456 PROGRAM-ID.                 HELLO.
        123456 AUTHOR. WEDEMANN / Softwork.app
        123456 INSTALLATION. Softwork.app
        123456 DATE-WRITTEN TODAY.
        123456******************************************************************
        123456 DATA                        DIVISION.
        123456 WORKING-STORAGE SECTION.
        123456 77 WORLD PIC X(6) VALUE 'WORLD!'
        123456 77 HELLO PIC X(6) VALUE 'HELLO'.
        123456/*****************************************************************
        123456 PROCEDURE                   DIVISION.
        123456******************************************************************
        123456     DISPLAY HELLO WORLD
        123456     PERFORM FOO.
        123456
        123456 FOO SECTION.
        123456 * Some Comment
        123456     MOVE "42" TO WORLD
        123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()
            .toIR()
            .optimize()

        val output = KotlinGenerator.generate(input)

        //language=kotlin
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        private fun foo(): Unit {
          world = "42"
          println("ANSWER${'$'}world")
        }

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
