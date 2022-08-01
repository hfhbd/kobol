package app.softwork.kobol

import kotlin.experimental.*

class Builder<T> : MutableList<T> by mutableListOf() {
    operator fun T.unaryPlus() = add(this)
}

@OptIn(ExperimentalTypeInference::class)
fun <T> build(@BuilderInference builder: Builder<T>.() -> Unit) = Builder<T>().apply(builder).toList()
