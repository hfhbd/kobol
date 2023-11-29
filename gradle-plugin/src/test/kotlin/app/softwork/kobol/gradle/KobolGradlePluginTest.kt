package app.softwork.kobol.gradle

import app.softwork.kobol.*
import app.softwork.kobol.flowgraph.*
import app.softwork.kobol.generator.java.*
import app.softwork.kobol.generator.kotlin.*
import app.softwork.kobol.java.java8.*
import app.softwork.kobol.plugins.ir.optimizations.*
import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import java.io.*
import java.nio.file.*
import kotlin.test.*

class KobolGradlePluginTest {
    @Test
    fun testConvertingKotlin() {
        val temp = Files.createTempDirectory("cobolTesting")
        val tempFile = temp.toFile()
        val cobolFile = File(tempFile, "hello.cbl").apply {
            writeText(input)
        }
        ExecuteKobol(
            rootPath = temp,
            input = setOf(cobolFile),
            outputFolder = tempFile,
            irPlugins = listOf(NoSynthetics()),
            codeGeneratorFactory = KotlinCodeGeneratorFactory(),
        )
        val packageFolder = File(tempFile, "kotlin/hello")
        assertTrue(packageFolder.exists())
        assertEquals(listOf("hello.kt"), packageFolder.list()?.toList())
    }

    @Test
    fun testConvertingJava() {
        val temp = Files.createTempDirectory("cobolTesting")
        val tempFile = temp.toFile()
        val cobolFile = File(tempFile, "hello.cbl").apply {
            writeText(input)
        }
        ExecuteKobol(
            rootPath = temp,
            input = setOf(cobolFile),
            outputFolder = tempFile,
            irPlugins = listOf(Java8Plugin(), NoSynthetics()),
            codeGeneratorFactory = JavaCodeGeneratorFactory(),
        )
        val packageFolder = File(tempFile, "java/hello")
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
            123456* Some Comment
            123456     DISPLAY "HELLO"WORLD
            123456     MOVE "42" TO WORLD
            123456     DISPLAY "ANSWER"WORLD.
    """.trimIndent()

    @Test
    fun testFlowGraph() {
        val temp = Files.createTempDirectory("cobolTesting")
        val tempFile = temp.toFile()
        val cobolFile = File(tempFile, "hello.cbl").apply {
            writeText(input)
        }

        PlantumlFlowGraph(tempFile).use {
            it.generate(listOf(cobolFile.toTree(temp)))
        }
        assertTrue("hello.puml" in tempFile.list())
    }

    @Test
    fun applyingKobolWorks() {
        val tmp = Files.createTempDirectory("cobolTesting").toFile()

        File(tmp, "build.gradle.kts").writeText(
            """
            |plugins {
            |  kotlin("jvm") version "1.9.0"
            |  id("app.softwork.kobol")
            |}
            |
            |repositories {
            |  mavenCentral()
            |}
            |
            |dependencies {
            |  kobol("org.jetbrains.kotlin:kotlin-stdlib-jdk8:1.8.0")
            |}
            |
            |sourceSets.main {
            |  cobol {
            |  }
            |}        
            |
            """.trimMargin(),
        )

        val result = GradleRunner.create()
            .withProjectDir(tmp)
            .withPluginClasspath()
            .withArguments(":help", "--configuration-cache")
            .build()

        assertEquals(TaskOutcome.SUCCESS, result.task(":help")?.outcome)
    }
}
