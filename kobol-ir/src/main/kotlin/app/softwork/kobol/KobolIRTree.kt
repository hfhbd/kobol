package app.softwork.kobol

data class KobolIRTree(val name: String, val main: Types.Function, val types: List<Types>) {
    sealed interface Types {
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

            sealed interface Statement {
                val comments: List<String>

                sealed interface Declaration : Statement {
                    val name: String
                    val mutable: Boolean
                    val private: Boolean
                    val value: Expression?

                    sealed interface Primitive : Declaration

                    data class StringDeclaration(
                        override val name: String,
                        override val value: Expression.StringExpression?,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String>
                    ) : Primitive
                }

                data class Assignment(
                    val declaration: Declaration,
                    val newValue: Expression,
                    override val comments: List<String>
                ) : Statement

                data class FunctionCall(
                    val function: Function,
                    val parameters: List<Declaration>,
                    override val comments: List<String>
                ) : Statement, Expression

                data class Print(val expr: Expression.StringExpression, override val comments: List<String>) : Statement

                data class Exit(override val comments: List<String>) : Statement

                data class LoadExternal(val libName: String) : Statement {
                    override val comments: List<String> = emptyList()
                }
            }
        }

        sealed interface Type : Types {
            data class GlobalVariable(
                val declaration: Function.Statement.Declaration,
                val const: Boolean,
                val doc: List<String>
            ) : Type

            data class Class(
                val name: String,
                val constructor: Constructor,
                val members: List<Function.Statement.Declaration>,
                val functions: List<Function>,
                val doc: List<String>,
                val init: List<Function.Statement>
            ) : Type {
                data class Constructor(val parameters: List<Function.Statement.Declaration>)
            }

            object Void : Type

            data class External(val name: String, val qualifiedName: String) : Type
        }
    }

    sealed interface Expression {
        sealed interface Literal : Expression {
            val value: Any
        }

        sealed interface Variable : Expression {
            val target: Types.Function.Statement.Declaration
        }

        sealed interface StringExpression : Expression {
            data class StringLiteral(override val value: String) : StringExpression, Literal
            data class StringVariable(override val target: Types.Function.Statement.Declaration.StringDeclaration) :
                StringExpression, Variable

            data class Concat(val left: Expression, val right: Expression) : StringExpression
        }
    }
}
