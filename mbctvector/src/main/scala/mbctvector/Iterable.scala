package mbctvector

trait Iterable[T] {
  def iterator: Iterator[T]
	
  def foreach(f: (T)=>Unit) = {
	val it = iterator
	while (it.hasNext) {
	  f(it.next)
	}
  }
}

abstract class Iterator[T] {
  def next: T
  def hasNext: Boolean
}