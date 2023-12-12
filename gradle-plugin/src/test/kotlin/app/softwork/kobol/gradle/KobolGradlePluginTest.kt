package app.softwork.kobol.gradle

import app.softwork.kobol.*
import app.softwork.kobol.flowgraph.*
import app.softwork.kobol.generator.java.*
import app.softwork.kobol.generator.kotlin.*
import app.softwork.kobol.ir.CobolTestFixtures
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
        val tempFile = Files.createTempDirectory("cobolTesting").toFile()
        val simple by CobolTestFixtures
        ExecuteKobol(
            input = listOf(simple),
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
        val simple by CobolTestFixtures
        ExecuteKobol(
            input = listOf(simple),
            outputFolder = tempFile,
            irPlugins = listOf(Java8Plugin(), NoSynthetics()),
            codeGeneratorFactory = JavaCodeGeneratorFactory(),
        )
        val packageFolder = File(tempFile, "java/hello")
        assertTrue(packageFolder.exists())
        assertEquals(listOf("Hello.java"), packageFolder.list()?.toList())
    }

    @Test
    fun testFlowGraph() {
        val simple by CobolTestFixtures
        val temp = Files.createTempDirectory("cobolTesting")
        val tempFile = temp.toFile()

        PlantumlFlowGraph(tempFile).use {
            it.generate(listOf(simple.toTree()))
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
