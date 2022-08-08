package app.softwork.kobol

import kotlinx.serialization.*

@Serializable
data class KobolIRTree(val name: String, val main: Types.Function, val types: List<Types>) {
    @Serializable
    sealed interface Types {
        @Serializable
        data class Function(
            val name: String,
            val parameters: List<Statement.Declaration> = emptyList(),
            val returnType: Type,
            val body: List<Statement>,
            val private: Boolean = false,
            val doc: List<String> = emptyList(),
            val external: Boolean = false
        ) : Types {

            fun declaration() = Function(
                name = name,
                parameters = parameters,
                returnType = returnType,
                body = emptyList(),
                private = private,
                doc = doc
            )

            constructor(
                name: String,
                parameters: List<Statement.Declaration> = emptyList(),
                returnType: Type = Type.Void,
                private: Boolean = false,
                doc: List<String> = emptyList(),
                body: Builder<Statement>.() -> Unit
            ) : this(
                name, parameters, returnType, build(body), private, doc
            )

            @Serializable
            sealed interface Statement {
                val comments: List<String>

                @Serializable
                sealed interface Declaration : Statement {
                    val name: String
                    val mutable: Boolean
                    val private: Boolean
                    val value: Expression?

                    @Serializable
                    sealed interface Primitive : Declaration

                    @Serializable
                    data class StringDeclaration(
                        override val name: String,
                        override val value: Expression.StringExpression?,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String>
                    ) : Primitive
                }

                @Serializable
                data class Assignment(
                    val declaration: Declaration,
                    val newValue: Expression,
                    override val comments: List<String>
                ) : Statement

                @Serializable
                data class FunctionCall(
                    val function: Function,
                    val parameters: List<Declaration>,
                    override val comments: List<String>
                ) : Statement, Expression

                @Serializable
                data class Print(val expr: Expression.StringExpression, override val comments: List<String>) : Statement

                @Serializable
                data class Exit(override val comments: List<String>) : Statement

                @Serializable
                data class LoadExternal(val libName: String) : Statement {
                    override val comments: List<String> = emptyList()
                }
            }
        }

        @Serializable
        sealed interface Type : Types {
            @Serializable
            data class GlobalVariable(
                val declaration: Function.Statement.Declaration,
                val const: Boolean,
                val doc: List<String>
            ) : Type

            @Serializable
            data class Class(
                val name: String,
                val constructor: Constructor,
                val members: List<Function.Statement.Declaration>,
                val functions: List<Function>,
                val doc: List<String>,
                val init: List<Function.Statement>
            ) : Type {
                @Serializable
                data class Constructor(val parameters: List<Function.Statement.Declaration>)
            }

            @Serializable
            object Void : Type

            @Serializable
            data class External(val name: String, val qualifiedName: String) : Type
        }
    }

    @Serializable
    sealed interface Expression {
        @Serializable
        sealed interface Literal : Expression {
            val value: Any
        }

        @Serializable
        sealed interface Variable : Expression {
            val target: Types.Function.Statement.Declaration
        }

        @Serializable
        sealed interface StringExpression : Expression {
            @Serializable
            data class StringLiteral(override val value: String) : StringExpression, Literal

            @Serializable
            data class StringVariable(override val target: Types.Function.Statement.Declaration.StringDeclaration) :
                StringExpression, Variable

            @Serializable
            data class Concat(val left: Expression, val right: Expression) : StringExpression
        }
    }
}
