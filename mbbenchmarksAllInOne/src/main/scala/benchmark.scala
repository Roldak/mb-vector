import MbArray._
import scala.util._

// ITERABLE

trait Iterable[@miniboxed T] {
  def iterator: Iterator[T]

  def foreach(f: (T) => Unit) = {
    val it = iterator
    while (it.hasNext) {
      f(it.next)
    }
  }
}

abstract class Iterator[@miniboxed T] {
  def next: T
  def hasNext: Boolean
}

// BUILDABLE

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

// MBVECTOR

class MbVector[@miniboxed T](var _size: Int) extends Buildable[T, MbVector] {
  private var _capacity = MbVectorUtils.nextPowerOfTwo(_size) + 1
  private var _array = MbArray.empty[T](_capacity)
  println(_array.getClass)

  def clear() = {
    _capacity = 1
    _size = 0
    _array = MbArray.empty(_capacity)
  }

  def capacity = _capacity

  def length = _size

  def add(elem: T) = {
    if (_size >= _capacity) {
      val copy = _array
      _capacity *= 2
      _array = MbArray.empty[T](_capacity)
      MbVectorUtils.copyAll(copy, _array);
    }

    _array(_size) = elem

    _size += 1
  }

  def remove(index: Int) = {
    checkIndexBounds(index)

    MbVectorUtils.shiftLeft(_array, index)

    _size -= 1
  }

  def apply(index: Int): T = {
    checkIndexBounds(index)
    _array(index)
  }

  def update(index: Int, elem: T) = {
    checkIndexBounds(index)
    _array(index) = elem
  }

  override def iterator = new MbVectorIterator[T](this)
  override def builder[@miniboxed K] = new MbVectorBuilder[K]

  override def toString = {
    var str = "{"
    var i = 0
    while (i < _size) {
      str += _array(i).toString() + (if (i != _size - 1) ", " else "")
      i += 1
    }
    str + "}"
  }

  private def checkIndexBounds(index: Int) = {
    assert(index >= 0)
    assert(index < _size)
  }
}

object MbVectorUtils {
  def nextPowerOfTwo(i: Int): Int = {
    var x = i
    x |= x >> 1
    x |= x >> 2
    x |= x >> 4
    x |= x >> 8
    x |= x >> 16
    x
  }

  def copyAll[@miniboxed T](from: MbArray[T], to: MbArray[T]) = {
    assert(from.length() < to.length())

    var i = 0
    val len = from.length()

    while (i < len) {
      to(i) = from(i)
      i += 1
    }
  }

  def shiftLeft[@miniboxed T](ary: MbArray[T], fromIndex: Int) = {
    var i = fromIndex
    val len = ary.length()

    while (i < len - 1) {
      ary(i) = ary(i + 1)
      i += 1
    }
  }
}

protected class MbVectorIterator[@miniboxed T](val vec: MbVector[T]) extends Iterator[T] {
  var i = 0

  override def next: T = {
    assert(i < vec.length)
    val elem = vec(i)
    i += 1
    elem
  }

  override def hasNext: Boolean = i < vec.length
}

protected class MbVectorBuilder[@miniboxed T] extends Builder[T, MbVector] {
  var innerVec: MbVector[T] = new MbVector[T](0)

  override def append(elem: T) = innerVec.add(elem)
  override def finalise = innerVec
}

// BENCHMARK

object Benchmark {
  
  def vecSize = 10000000
  def opCount = 10
  def rnd = new Random(42)
  
  def makeVector(size: Int) = {
    val vec = new MbVector[Int](size)
    var i = 0
    while (i < vec.length) {
      vec(i) = rnd.nextInt()
      i += 1
    }
    vec
  }
  
  def time(opName: String, count: Int, init: => MbVector[Int], operation: MbVector[Int] => Unit) = {
    var i = 1
    var total = 0L
    
    println("MbVector. " + opName + " : ")
    
    while (i <= count) {
      var vec = init
      
      System.gc()
      
      val start = System.currentTimeMillis()
      
      operation(vec)
      vec = null
      
      val end = System.currentTimeMillis()
      
      println("\t" + i + ". : " + (end - start) + "ms");

      System.gc()
      
      i += 1
      total += end - start
    }
    
    println("Total : " + total + "ms. Average " + (total.toDouble / count) + "ms.\n")
  }
  
  def main(args: Array[String]) = {
    
    time("map", opCount, {
      makeVector(vecSize)
    }, {
      _.map { _ * 2 }
    })
    
  }
}
