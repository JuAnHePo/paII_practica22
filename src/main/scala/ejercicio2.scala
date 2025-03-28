object ejercicio2 {
  trait InmutableQueue[T] {
    def enqueue(elem: T): InmutableQueue[T]
    def dequeue(): (T, InmutableQueue[T])
    def isEmpty: Boolean
  }

  class SimpleQueue[T] private(private val elems: List[T]) extends InmutableQueue[T] {
    def this(elems: T*) = this(elems.toList)

    override def enqueue(elem: T): InmutableQueue[T] = SimpleQueue[T](elems :+ elem)

    override def dequeue(): (T, InmutableQueue[T]) = {
      elems match
        case Nil => throw new NoSuchElementException("La cola está vacía.")
        case h :: t => (h, SimpleQueue(t))
    }

    override def isEmpty: Boolean = elems.isEmpty

    override def toString: String = s"SimpleQueue(${elems.mkString(", ")})"

    override def equals(obj: Any): Boolean = {
      obj match
        case that: SimpleQueue[T] => this.elems == that.elems
        case _ => false
    }

    override def hashCode(): Int = elems.hashCode()
  } 
}
