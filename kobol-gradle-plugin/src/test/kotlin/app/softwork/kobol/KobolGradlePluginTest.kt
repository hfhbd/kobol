package app.softwork.kobol

import app.softwork.kobol.generator.*
import org.gradle.testkit.runner.*
import java.io.*
import java.nio.file.*
import kotlin.io.path.*
import kotlin.test.*

class KobolGradlePluginTest {
    @Test
    fun testConverting() {
        //language=cobol
        val input = """
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
    fun srcDir() {
        val tmp = Files.createTempDirectory("cobolTesting").toFile().apply {
            deleteOnExit()
        }
        val buildFile = File(tmp, "build.gradle.kts")
        //language=kotlin
        buildFile.writeText(
            """
            plugins {
                kotlin("jvm") version "1.7.20-Beta" 
                id("app.softwork.kobol")
            }

            repositories {
                mavenCentral()
            }
            
            tasks {
              convertCobolToKotlin {
                source("hello.cbl")
              }
            }
        """.trimIndent()
        )

        File(tmp, "hello.cbl").writeText(
            //language=cobol
            """
            |000010 IDENTIFICATION DIVISION.
            |000020 PROGRAM-ID. HELLO.
            |000030 PROCEDURE DIVISION.
            |000040     DISPLAY "HELLO".
        """.trimMargin()
        )

        val kotlinSrc = (tmp.toPath() / "src" / "main" / "kotlin" / "hello").toFile()
        kotlinSrc.mkdirs()
        File(kotlinSrc, "Test.kt").writeText(
            //language=kotlin
            """
            package hello
            
            fun foo() {
              main()
            }
        """.trimIndent()
        )

        val result = GradleRunner.create()
            .withProjectDir(tmp)
            .withArguments("convertCobolToKotlin", "assemble", "--stacktrace")
            .forwardOutput()
            .withPluginClasspath()
            .build()

        assertEquals(TaskOutcome.SUCCESS, result.task(":convertCobolToKotlin")?.outcome)
        assertEquals(TaskOutcome.SUCCESS, result.task(":assemble")?.outcome)
    }
}
