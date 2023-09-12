package app.softwork.kobol

context(MutableList<T>)
public operator fun <T> T.unaryPlus(): Boolean = add(this)

context(MutableList<T>)
public operator fun <T> List<T>.unaryPlus(): Boolean = addAll(this)

public inline fun <T> build(builder: MutableList<T>.() -> Unit): MutableList<T> = buildList(builder).toMutableList()
