package app.softwork.kobol.generator

import java.io.*
import kotlin.test.*

class HelloWorldTest {
    @Test
    fun helloWorld() {
        val input = File(HelloWorldTest::class.java.classLoader.getResource("HELLO.cobol")!!.file)
        val output = KotlinGenerator.generate(input)
        val expected = """
        package hello
        
        import kotlin.String
        import kotlin.Unit
        
        public var WORLD: String = "WORLD!"
        
        public fun main(): Unit {
          println("HELLO${'$'}WORLD")
          WORLD = "42"
          println("ANSWER${'$'}WORLD")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
