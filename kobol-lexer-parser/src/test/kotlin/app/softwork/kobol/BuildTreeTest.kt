package app.softwork.kobol

import com.intellij.testFramework.*
import kotlin.test.*

@Ignore
class BuildTreeTest : ParsingTestCase("", "cobol", CobolParserDefinition) {
    @Test
    fun buildTree() {
        val input = """
            
        """.trimIndent()
        val file = parseFile("test", input) as CobolFile
        file.toTree()
    }
}
