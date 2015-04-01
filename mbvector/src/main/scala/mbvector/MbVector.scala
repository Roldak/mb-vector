package mbvector

import MbArray._

class MbVector[@miniboxed T](var _size: Int) extends Iterable[T] {
  private var _capacity = MbVector.nextPowerOfTwo(_size) + 1
  private var _array = MbArray.empty[T](_capacity)
  
  def clear() = {
    _capacity = 1
    _size = 0
    _array = MbArray.empty(_capacity)
  }
  
  def capacity = _capacity
  
  def length = _size
  
  def add(elem: T) = {
    if (_size >= _capacity) {      
      val copy = _array.clone()
      _capacity *= 2
      _array = MbArray.empty[T](_capacity)
      MbVector.copyAll(copy, _array);
    }
    
    _array(_size) = elem
    
    _size += 1
  }
  
  def remove(index: Int) = {
    checkIndexBounds(index)
    
    MbVector.shiftLeft(_array, index)
    
    _size -= 1
  }
  
  def apply(index: Int) = {
    checkIndexBounds(index)
    _array(index)
  }
  
  def update(index: Int, elem: T) = {
    checkIndexBounds(index)
    _array(index) = elem
  }
  
  override def iterator = new MbVectorIterator[T](this)
  
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

object MbVector {
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

class MbVectorIterator[@miniboxed T](val vec: MbVector[T]) extends Iterator[T] {
	var i = 0
	
	override def next: T = {
		assert(i < vec.length)
		val elem = vec(i)
		i += 1
		elem
	}
	
	override def hasNext: Boolean = i < vec.length
}

object Main {
  def main(args: Array[String]) = {
    val vec = new MbVector[Int](11)
	
	println(vec.capacity)
    vec(0) = 2
    vec(1) = -1
    vec(2) = 12
    vec(3) = 4
    vec(4) = 1202
	
	vec.foreach(x => println(x))
	
	for (i <- 0 to 50) {
		vec.add(2)
	}
	println(vec.capacity)
    println(vec)
	vec.remove(3)
	println(vec)
	vec.clear()
	println(vec)
  }
}