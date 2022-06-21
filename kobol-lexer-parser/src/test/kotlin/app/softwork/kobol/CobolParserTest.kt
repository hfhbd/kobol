package app.softwork.kobol

import com.intellij.testFramework.*
import kotlin.test.*

@Ignore
class CobolParserTest : ParsingTestCase("", "cobol", CobolParserDefinition) {
    @Test
    fun testInput() {
        doTest(true)
    }

    override fun getTestDataPath() = "src/test/resources"
}
