package mbvector

trait Buildable[@miniboxed T, Container[_]] extends Iterable[T] {
  def builder[@miniboxed K]: Builder[K, Container]
	
  def map[@miniboxed U](f: T => U) = {
    val bd = builder[U]
    val it = iterator
    while (it.hasNext) bd.append(f(it.next))
    bd.finalise
  }
  
  def filter(f: T => Boolean) = {
    val bd = builder[T]
    val it = iterator
    while (it.hasNext) {
	  val elem = it.next
	  if (f(elem)) {
	    bd.append(elem)
	  }
	}
    bd.finalise
  }
}

abstract class Builder[@miniboxed T, Container[_]] {
  def append(x: T)
  def finalise: Container[T]
}