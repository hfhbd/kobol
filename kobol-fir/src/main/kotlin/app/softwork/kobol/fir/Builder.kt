package app.softwork.kobol.fir

@JvmInline
public value class Builder<T> (private val list: MutableList<T> = mutableListOf()) : MutableList<T> by list {
    public inline operator fun T.unaryPlus(): Boolean = add(this)
    //public inline operator fun List<T>.unaryPlus(): Boolean = addAll(this)
}

public inline fun <T> build(builder: Builder<T>.() -> Unit): List<T> = Builder<T>().apply(builder).toList()
