package app.softwork.kobol

// TODO: Remove when serialization plugin supports K2

@JvmInline
public value class Builder<T> (private val list: MutableList<T> = mutableListOf()) : MutableList<T> by list {
    public inline operator fun T.unaryPlus(): Boolean = add(this)
    public inline operator fun List<T>.unaryPlus(): Boolean = addAll(this)
}

public inline fun <T> build(builder: Builder<T>.() -> Unit): MutableList<T> = Builder<T>().apply(builder).toMutableList()
public inline fun <T> build(builder: List<T>): MutableList<T> = builder.toMutableList()
