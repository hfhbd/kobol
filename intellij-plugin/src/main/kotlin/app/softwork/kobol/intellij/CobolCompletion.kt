package app.softwork.kobol.intellij

import app.softwork.kobol.CobolTypes.*
import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.lookup.*
import com.intellij.patterns.*
import com.intellij.patterns.PlatformPatterns.*
import com.intellij.psi.*
import com.intellij.psi.TokenType.*
import com.intellij.util.*

internal class CobolCompletion : CompletionContributor() {
    init {
        extend(CompletionType.BASIC, psiElement(VARNAME)) { parameters, _ ->
            when (parameters.position.text.uppercase()) {
                "DISPLAY" -> LookupElementBuilder.create("Hello")
                else -> LookupElementBuilder.create("Hello 2")
            }
        }
        extend(CompletionType.BASIC, psiElement(WHITE_SPACE).afterSibling(psiElement(DISPLAY))) { _, _ ->
            LookupElementBuilder.create("Hello DISPLAY")
        }
        extend(
            CompletionType.BASIC,
            psiElement(WHITE_SPACE).afterSibling(psiElement(WHITE_SPACE).afterSibling(psiElement(DISPLAY))),
        ) { _, _ ->
            LookupElementBuilder.create("Hello DISPLAY 2")
        }
        extend(
            CompletionType.BASIC,
            psiElement(VARNAME).afterSibling(psiElement(WHITE_SPACE).afterSibling(psiElement(DISPLAY))),
        ) { _, _ ->
            LookupElementBuilder.create("Hello DISPLAY 2")
        }

        extend(
            CompletionType.BASIC,
            psiElement(DISPLAY).afterSibling(psiElement(WHITE_SPACE).afterSibling(psiElement(WHITE_SPACE))),
        ) { _, _ ->
            LookupElementBuilder.create("DISPLAY 2")
        }
        extend(CompletionType.BASIC, psiElement(PROCEDURES)) { _, _ ->
            LookupElementBuilder.create("DISPLAY PROCEDURES")
        }

        extend(
            CompletionType.BASIC,
            psiElement(PROCEDURE),
            object : CompletionProvider<CompletionParameters>() {
                override fun addCompletions(
                    parameters: CompletionParameters,
                    context: ProcessingContext,
                    resultSet: CompletionResultSet,
                ) {
                    resultSet.addElement(LookupElementBuilder.create("DIVISION"))
                }
            },
        )
    }
}

private fun CompletionContributor.extend(
    completionType: CompletionType,
    elementPattern: ElementPattern<PsiElement>,
    completion: (CompletionParameters, ProcessingContext) -> LookupElement,
) {
    extend(
        completionType,
        elementPattern,
        object : CompletionProvider<CompletionParameters>() {
            override fun addCompletions(
                parameters: CompletionParameters,
                context: ProcessingContext,
                resultSet: CompletionResultSet,
            ) {
                resultSet.addElement(completion(parameters, context))
            }
        },
    )
}
