package app.softwork.kobol.compiler

import java.io.*
import kotlin.test.*

class HelloWorldTest {
    @Test
    fun helloWorld() {
        val input = File(HelloWorldTest::class.java.classLoader.getResource("HELLO.cobol")!!.file)
        val output = KobolCompiler.generateMain(input)
        val expected = """
        package hello
        
        import kotlin.Unit
        
        public fun main(): Unit {
          println("HELLO")
          println("WORLD!")
        }
        
        """.trimIndent()
        assertEquals(expected, output.toString())
    }
}
