package ctvector

import scala.reflect._

trait Buildable[T, Container[_]] extends Iterable[T] {
  def builder[K : ClassTag]: Builder[K, Container]

  def map[U : ClassTag](f: T => U) = {
    val bd = builder[U]
    val it = iterator
    while (it.hasNext) bd.append(f(it.next))
    bd.finalise
  }

  def filter(f: T => Boolean)(implicit tag: ClassTag[T]) = {
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

abstract class Builder[T, Container[_]] {
  def append(x: T)
  def finalise: Container[T]
}