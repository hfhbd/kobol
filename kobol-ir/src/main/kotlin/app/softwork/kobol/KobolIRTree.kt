package app.softwork.kobol

data class KobolIRTree(val name: String, val main: Types.Function, val types: List<Types>) {
    sealed interface Types {
        data class Function(
            val name: String,
            val parameters: List<Statement.Declaration>,
            val returnType: Type,
            val body: List<Statement>,
            val private: Boolean
        ) : Types {
            sealed interface Statement {
                sealed interface Declaration : Statement {
                    val name: String
                    val modifier: Modifier
                    val value: Expression?

                    enum class Modifier {
                        Write, ReadOnly
                    }

                    data class StringDeclaration(
                        override val name: String,
                        override val value: Expression.StringExpression?,
                        override val modifier: Modifier
                    ) : Declaration
                }

                data class Assignment(val declaration: Declaration, val newValue: Expression) : Statement

                data class FunctionCall(val function: Function, val parameters: List<Declaration>) : Statement,
                    Expression

                data class Print(val expr: Expression.StringExpression): Statement
            }
        }

        sealed interface Type : Types {
            data class GlobalVariable(val declaration: Function.Statement.Declaration) : Type
            data class Class(
                val name: String,
                val constructor: Constructor,
                val members: kotlin.collections.List<Function.Statement.Declaration>,
                val functions: kotlin.collections.List<Function>
            ) : Type {
                data class Constructor(val parameters: kotlin.collections.List<Function.Statement.Declaration>)
            }

            object Void: Type

            data class List(val type: Type) : Type

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

            data class Concat(val left: StringExpression, val right: StringExpression) : StringExpression
        }
    }
}
