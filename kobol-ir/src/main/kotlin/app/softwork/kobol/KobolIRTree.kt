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

                    @Serializable
                    sealed interface NumberDeclaration : Primitive

                    @Serializable
                    data class IntDeclaration(
                        override val name: String,
                        override val value: Expression.NumberExpression.IntExpression?,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String>
                    ) : NumberDeclaration

                    @Serializable
                    data class DoubleDeclaration(
                        override val name: String,
                        override val value: Expression.NumberExpression.DoubleExpression?,
                        override val mutable: Boolean,
                        override val private: Boolean,
                        override val comments: List<String>
                    ) : NumberDeclaration

                    @Serializable
                    data class BooleanDeclaration(
                        override val name: String,
                        override val value: Expression.BooleanExpression?,
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

                @Serializable
                data class DoWhile(
                    val functionCall: FunctionCall,
                    val condition: Expression.BooleanExpression,
                    override val comments: List<String>
                ) : Statement, Expression

                @Serializable
                data class While(
                    val condition: Expression.BooleanExpression,
                    val statements: List<Statement> = emptyList(),
                    override val comments: List<String>
                ) : Statement, Expression

                @Serializable
                data class ForEach(
                    val counter: Declaration.NumberDeclaration,
                    val from: Expression.NumberExpression,
                    val step: Expression.NumberExpression? = null,
                    val condition: Expression.BooleanExpression,
                    val statements: List<Statement> = emptyList(),
                    override val comments: List<String>
                ) : Statement, Expression

                @Serializable
                data class If(
                    val condition: Expression.BooleanExpression,
                    val statements: List<Statement>,
                    val elseStatements: List<Statement> = emptyList(),
                    override val comments: List<String> = emptyList()
                ): Statement, Expression
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
                val init: List<Function.Statement>,
                val isObject: Boolean
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

            @Serializable
            data class Interpolation(val expr: Expression): StringExpression
        }

        @Serializable
        sealed interface NumberExpression : Expression {

            @Serializable
            sealed interface IntExpression : NumberExpression {
                @Serializable
                data class IntLiteral(override val value: Int) : IntExpression, Literal

                @Serializable
                data class IntVariable(override val target: Types.Function.Statement.Declaration.IntDeclaration) :
                    IntExpression, Variable
            }

            @Serializable
            sealed interface DoubleExpression : NumberExpression {
                @Serializable
                data class DoubleLiteral(override val value: Double) : DoubleExpression, Literal

                @Serializable
                data class DoubleVariable(override val target: Types.Function.Statement.Declaration.DoubleDeclaration) :
                    DoubleExpression, Variable
            }
        }

        @Serializable
        sealed interface BooleanExpression : Expression {
            @Serializable
            data class BooleanLiteral(override val value: Boolean) : BooleanExpression, Literal

            @Serializable
            data class Eq(val left: Expression, val right: Expression) : BooleanExpression

            @Serializable
            data class NotEq(val left: Expression, val right: Expression) : BooleanExpression

            @Serializable
            data class Not(val condition: BooleanExpression) : BooleanExpression

            @Serializable
            data class Or(val left: BooleanExpression, val right: BooleanExpression) : BooleanExpression

            @Serializable
            data class And(val left: BooleanExpression, val right: BooleanExpression) : BooleanExpression

            @Serializable
            data class Bigger(val left: NumberExpression, val right: NumberExpression, val equals: Boolean = false) :
                BooleanExpression

            @Serializable
            data class Smaller(val left: NumberExpression, val right: NumberExpression, val equals: Boolean = false) :
                BooleanExpression
        }
    }
}
