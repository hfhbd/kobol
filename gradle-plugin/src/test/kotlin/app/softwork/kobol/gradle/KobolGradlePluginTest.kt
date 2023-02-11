package app.softwork.kobol.gradle

import app.softwork.kobol.fir.*
import app.softwork.kobol.flowgraph.*
import app.softwork.kobol.generator.java.*
import app.softwork.kobol.generator.kotlin.*
import app.softwork.kobol.java.java8.*
import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
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
        ExecuteKobol(
            input = setOf(cobolFile),
            outputFolder = tmp,
            codeGeneratorFactory = KotlinCodeGeneratorFactory()
        )
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
        ExecuteKobol(
            input = setOf(cobolFile),
            outputFolder = tmp,
            irPlugins = listOf(Java8Plugin()),
            codeGeneratorFactory = JavaCodeGeneratorFactory()
        )
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

        PlantumlFlowGraph(tmp).use {
            it.invoke(cobolFile.toTree(), emptyList())
        }
        assertTrue("hello.puml" in tmp.list())
    }

    @Test
    fun customFlowGraph() {
        val tmp = Files.createTempDirectory("cobolTesting").toFile().apply {
            deleteOnExit()
        }
        File(tmp, "build.gradle.kts").writeText(
            """
            |import app.softwork.kobol.gradle.kobolPlugin
            |
            |plugins {
            |  kotlin("jvm") version "1.8.10"
            |  id("app.softwork.kobol")
            |}
            |
            |repositories {
            |  mavenCentral()
            |}
            |
            |dependencies {
            |  kobolPlugin(cobol.foo, "org.jetbrains.kotlin:kotlin-stdlib-jdk8:1.8.0")
            |}
            |
            |cobol.foo {
            |  dependencies {
            |    plugin("org.jetbrains.kotlin:kotlin-stdlib-jdk8:1.8.0")
            |  }
            |}
            |
            |tasks {
            |  uploadCobol {
            |    files(cobol.foo)
            |  }
            |}
            |
            """.trimMargin()
        )
        File(tmp, "foo.cbl").writeText(input)

        val result = GradleRunner.create()
            .withProjectDir(tmp)
            .withPluginClasspath()
            .withArguments(":flowGraph", "--configuration-cache")
            .build()

        assertEquals(TaskOutcome.SUCCESS, result.task(":flowGraph")?.outcome)
    }
}
