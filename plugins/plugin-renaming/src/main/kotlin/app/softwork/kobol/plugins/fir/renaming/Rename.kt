package app.softwork.kobol.plugins.fir.renaming

import app.softwork.kobol.fir.*
import app.softwork.kobol.fir.CobolFIRTree.DataTree.*
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.BooleanExpression
import app.softwork.kobol.fir.CobolFIRTree.ProcedureTree.Expression.Variable

public abstract class Rename(
    private val functions: String.() -> String,
    private val variables: String.() -> String,
    private val classes: String.() -> String,
) : FirPluginBeforePhase {
    public companion object {
        public val next: Regex = "-(.)".toRegex()

        public fun String.toCamelCase(): String = lowercase().replace(next) {
            it.groups[1]!!.value.uppercase()
        }

        public fun String.toPascalCase(): String = toCamelCase().replaceFirstChar { it.uppercase() }

        public fun String.toSnakeCase(): String = lowercase().replace("-", "_").uppercase()
    }

    override fun invoke(tree: CobolFIRTree): CobolFIRTree {
        return tree.copy(
            env = tree.env?.let {
                it.copy(
                    inputOutput = it.inputOutput?.let {
                        it.copy(
                            fileControl = it.fileControl?.let {
                                it.copy(
                                    files = it.files.map {
                                        it.copy(
                                            fileStatus = it.fileStatus?.variables(),
                                        )
                                    },
                                )
                            },
                        )
                    },
                )
            },
            data = tree.data.let {
                it.copy(
                    fileSection = it.fileSection.map {
                        it.rename()
                    },
                    workingStorage = it.workingStorage.rename(),
                    linkingSection = it.linkingSection.rename(),
                )
            },
            procedure = tree.procedure.copy(
                topLevel = tree.procedure.topLevel.rename(),
                sections = tree.procedure.sections.map {
                    it.copy(
                        name = it.name.functions(),
                        statements = it.statements.rename(),
                    )
                },
            ),
        )
    }

    private fun File.rename() = copy(
        records = records.rename() as List<WorkingStorage.Record>,
        fileStatus = fileStatus?.variables(),
    )

    @JvmName("renameStatements")
    private fun List<CobolFIRTree.ProcedureTree.Statement>.rename(): List<CobolFIRTree.ProcedureTree.Statement> {
        return map {
            when (it) {
                is CobolFIRTree.ProcedureTree.Statement.Call -> it.copy(
                    name = it.name.functions(),
                    parameters = it.parameters.rename(),
                )

                is CobolFIRTree.ProcedureTree.Statement.Close -> it.copy(
                    files = it.files.map { it.rename() },
                )

                is CobolFIRTree.ProcedureTree.Statement.Continue -> it
                is CobolFIRTree.ProcedureTree.Statement.Display -> it.copy(
                    expr = it.expr.rename() as Expression.StringExpression,
                )

                is CobolFIRTree.ProcedureTree.Statement.Eval -> it.copy(
                    values = it.values.rename(),
                    conditions = it.conditions.map {
                        it.copy(
                            conditions = it.conditions.rename(),
                            action = it.action.rename(),
                        )
                    },
                    other = it.other?.let {
                        it.copy(
                            action = it.action.rename(),
                        )
                    },
                )

                is CobolFIRTree.ProcedureTree.Statement.ForEach -> it.copy(
                    variable = it.variable.rename() as WorkingStorage.Elementar.NumberElementar,
                    from = it.from.rename() as Expression.NumberExpression,
                    by = it.by?.rename() as Expression.NumberExpression?,
                    until = it.until.rename() as BooleanExpression,
                    statements = it.statements.rename(),
                )

                is CobolFIRTree.ProcedureTree.Statement.GoBack -> it
                is CobolFIRTree.ProcedureTree.Statement.StopRun -> it
                is CobolFIRTree.ProcedureTree.Statement.If -> it.copy(
                    condition = it.condition.rename() as BooleanExpression,
                    statements = it.statements.rename(),
                    elseStatements = it.elseStatements.rename(),
                )

                is CobolFIRTree.ProcedureTree.Statement.Move -> it.copy(
                    target = it.target.rename() as WorkingStorage.Elementar,
                    value = it.value.rename(),
                )
                is CobolFIRTree.ProcedureTree.Statement.Add -> it.copy(
                    target = it.target.rename() as WorkingStorage.Elementar,
                    value = it.value.rename(),
                )
                is CobolFIRTree.ProcedureTree.Statement.Sub -> it.copy(
                    target = it.target.rename() as WorkingStorage.Elementar,
                    value = it.value.rename(),
                )

                is CobolFIRTree.ProcedureTree.Statement.Open -> it.copy(
                    file = it.file.rename(),
                )

                is CobolFIRTree.ProcedureTree.Statement.Perform -> it.copy(
                    sectionName = it.sectionName.functions(),
                    until = it.until?.rename() as BooleanExpression?,
                )

                is CobolFIRTree.ProcedureTree.Statement.Read -> it.copy(
                    file = it.file.rename(),
                    action = it.action.rename(),
                    atEnd = it.atEnd.rename(),
                )

                is CobolFIRTree.ProcedureTree.Statement.Sql -> it.copy(
                    updatingHostVariables = it.updatingHostVariables.rename() as List<Variable>,
                    parameter = it.parameter.rename() as List<Variable>,
                )

                is CobolFIRTree.ProcedureTree.Statement.While -> it.copy(
                    statements = it.statements.rename(),
                    until = it.until.rename() as BooleanExpression,
                )

                is CobolFIRTree.ProcedureTree.Statement.Write -> it.copy(
                    file = it.file.rename(),
                    from = it.from?.rename(),
                )
            }
        }
    }

    @JvmName("renameExpression")
    private fun List<Expression>.rename(): List<Expression> {
        return map { it.rename() }
    }

    private fun Expression.rename(): Expression {
        return when (this) {
            is BooleanExpression.And ->
                copy(
                    left = left.rename() as BooleanExpression,
                    right = right.rename() as BooleanExpression,
                )

            is BooleanExpression.Equals -> copy(
                left = left.rename(),
                right = right.rename(),
            )

            is BooleanExpression.Greater -> copy(
                left = left.rename() as Expression.NumberExpression,
                right = right.rename() as Expression.NumberExpression,
            )

            is BooleanExpression.Not -> copy(
                target = target.rename() as BooleanExpression,
            )

            is BooleanExpression.Or -> copy(
                left = left.rename() as BooleanExpression,
                right = right.rename() as BooleanExpression,
            )

            is BooleanExpression.Smaller -> copy(
                left = left.rename() as Expression.NumberExpression,
                right = right.rename() as Expression.NumberExpression,
            )

            is Expression.NumberExpression.NumberLiteral -> this
            is Expression.StringExpression.StringLiteral -> this
            is Expression.NumberExpression.NumberVariable -> copy(
                target = target.rename() as WorkingStorage.Elementar.NumberElementar,
            )

            is Expression.StringExpression.Concat -> copy(
                left = left.rename(),
                right = right.rename(),
            )

            is Expression.StringExpression.Interpolation -> copy(
                value = value.rename(),
            )

            is Expression.StringExpression.StringVariable -> copy(
                target = target.rename() as WorkingStorage.Elementar.StringElementar,
            )
        }
    }

    private fun List<WorkingStorage>.rename(): List<WorkingStorage> = map { it.rename() }

    private fun WorkingStorage.rename(): WorkingStorage = when (this) {
        is WorkingStorage.Elementar.EmptyElementar -> copy(
            name = name.variables(),
            recordName = recordName?.classes(),
        )

        is WorkingStorage.Elementar.NumberElementar.Normal -> copy(
            name = name.variables(),
            recordName = recordName?.classes(),
        )
        is WorkingStorage.Elementar.NumberElementar.ReturnCode -> copy(
            name = name.variables(),
        )

        is WorkingStorage.Elementar.Pointer -> copy(
            name = name.variables(),
            recordName = recordName?.classes(),
        )

        is WorkingStorage.Elementar.StringElementar -> copy(
            name = name.variables(),
            recordName = recordName?.classes(),
        )

        is WorkingStorage.Record -> copy(
            name = name.classes(),
        )
    }
}
