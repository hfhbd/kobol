package app.softwork.kobol.gradle

import app.softwork.kobol.fir.*
import app.softwork.kobol.flowgraph.*
import app.softwork.kobol.generator.kotlin.*
import java.io.*
import java.nio.file.*
import kotlin.test.*

class KobolGradlePluginTest {
    @Test
    fun testConvertingKotlin() {
        val tmp = Files.createTempDirectory("cobolTesting").toFile().apply {
            deleteOnExit()
        }
        val cobolFile = File(tmp, "hello.cbl").apply {
            writeText(input)
        }
        generate(cobolFile, tmp, optimize = false)
        val packageFolder = File(tmp, "kotlin/hello")
        assertTrue(packageFolder.exists())
        assertEquals(listOf("hello.kt"), packageFolder.list()?.toList())
    }

    @Test
    fun testConvertingJava() {
        val tmp = Files.createTempDirectory("cobolTesting").toFile().apply {
            deleteOnExit()
        }
        val cobolFile = File(tmp, "hello.cbl").apply {
            writeText(input)
        }
        app.softwork.kobol.generator.java.generate(cobolFile, tmp, optimize = false, java8 = true)
        val packageFolder = File(tmp, "java/hello")
        assertTrue(packageFolder.exists())
        assertEquals(listOf("Hello.java"), packageFolder.list()?.toList())
    }

    //language=cobol
    private val input = """
            123456 IDENTIFICATION              DIVISION.
            123456******************************************************************
            123456 PROGRAM-ID.                 HELLO.
            123456 INSTALLATION. Softwork.app
            123456 AUTHOR. WEDEMANN / Softwork.app
            123456 DATE-WRITTEN TODAY.
            123456******************************************************************
            123456 DATA                        DIVISION.
            123456 WORKING-STORAGE SECTION.
            123456 77 WORLD PIC X(6) VALUE 'WORLD!'.
            123456/*****************************************************************
            123456 PROCEDURE                   DIVISION.
            123456******************************************************************
            123456
            123456 * Some Comment
            123456     DISPLAY "HELLO"WORLD
            123456     MOVE "42" TO WORLD
            123456     DISPLAY "ANSWER"WORLD.
        """.trimIndent()

    @Test
    fun testFlowGraph() {
        val tmp = Files.createTempDirectory("cobolTesting").toFile().apply {
            deleteOnExit()
        }
        val cobolFile = File(tmp, "hello.cbl").apply {
            writeText(input)
        }

        FlowGraph(tmp).use {
            it.invoke(cobolFile.toTree())
        }
        assertTrue("hello.puml" in tmp.list())
    }
}
