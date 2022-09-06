package app.softwork.kobol

// TODO: Use Compose

class Builder<T> : MutableList<T> by mutableListOf() {
    operator fun T.unaryPlus() = add(this)
}

fun <T> build(builder: Builder<T>.() -> Unit) = Builder<T>().apply(builder).toList()
