package app.softwork.kobol

context(list: MutableList<T>)
public operator fun <T> T.unaryPlus(): Boolean = list.add(this)

context(list: MutableList<T>)
public operator fun <T> List<T>.unaryPlus(): Boolean = list.addAll(this)

public inline fun <T> build(builder: MutableList<T>.() -> Unit): MutableList<T> = buildList(builder).toMutableList()
