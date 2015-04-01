package ctvector

class CtVector[T : ClassTag](var _size: Int) extends Buildable[T, CtVector] {
  private var _capacity = CtVectorUtils.nextPowerOfTwo(_size) + 1
  private var _array = new Array[T](_capacity)
  
  def clear() = {
    _capacity = 1
    _size = 0
    _array = Array.empty(_capacity)
  }
  
  def capacity = _capacity
  
  def length = _size
  
  def add(elem: T) = {
    if (_size >= _capacity) {      
      val copy = _array.clone()
      _capacity *= 2
      _array = new Array(_capacity)
      CtVectorUtils.copyAll(copy, _array);
    }
    
    _array(_size) = elem
    
    _size += 1
  }
  
  def remove(index: Int) = {
    checkIndexBounds(index)
    
    CtVectorUtils.shiftLeft(_array, index)
    
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
  
  override def iterator = new CtVectorIterator[T](this)
  override def builder[K] = new CtVectorBuilder[K]
  
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

protected object CtVectorUtils {
  def nextPowerOfTwo(i: Int): Int = {
    var x = i
    x |= x >> 1
    x |= x >> 2
    x |= x >> 4
    x |= x >> 8
    x |= x >> 16
    x
  }
  
  def copyAll[T](from: Array[T], to: Array[T]) = {
    assert(from.length() < to.length())
    
    var i = 0
    val len = from.length()
    
    while (i < len) {
      to(i) = from(i)
      i += 1
    }
  }
  
  def shiftLeft[T](ary: Array[T], fromIndex: Int) = {
    var i = fromIndex
    val len = ary.length()
    
    while (i < len - 1) {
      ary(i) = ary(i + 1)
      i += 1
    }
  }
}

protected class CtVectorIterator[T](val vec: CtVector[T]) extends Iterator[T] {
  var i = 0
	
  override def next: T = {
	assert(i < vec.length)
	val elem = vec(i)
	i += 1
	elem
  }
	
  override def hasNext: Boolean = i < vec.length
}

protected class CtVectorBuilder[T] extends Builder[T, CtVector] {
  var innerVec: CtVector[T] = new CtVector[T](0)
  
  override def append(elem: T) = innerVec.add(elem)
  override def finalise = innerVec
}

object Main {
  def main(args: Array[String]) = {
    val vec = new CtVector[Int](11)
	
	println(vec.capacity)
    vec(0) = 2
    vec(1) = -1
    vec(2) = 12
    vec(3) = 4
    vec(4) = 1202
	
	vec.map(x => 2 * x).foreach(x => println(x))
	
	for (i <- 0 to 10) {
		vec.add(2)
	}
	
	println(vec.capacity)
    println(vec)
	
	vec.remove(3)
	
	println(vec)
	
	vec.filter(x => x%2 == 0).foreach(x => println(x))
	
	vec.clear()
	
	println(vec)
  }
}