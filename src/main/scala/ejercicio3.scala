import ejercicio2.InmutableQueue

object ejercicio3 {
  class EfficientQueue[T] private (private val front: List[T], private val rear: List[T]) extends InmutableQueue[T] {
    def this(elems: T*) = this(elems.toList, Nil)

    override def enqueue(elem: T): EfficientQueue[T] = EfficientQueue(front, rear :+ elem)

    override def dequeue(): (T, EfficientQueue[T]) = {
      front match
        case h::t => (h, EfficientQueue(t, rear))
        case Nil => (rear.last, EfficientQueue(rear.init.reverse, Nil))
    }

    override def isEmpty: Boolean = front.isEmpty && rear.isEmpty

    override def toString: String = s"EfficientQueue(${(front ++ rear.reverse).mkString(", ")})"

    override def equals(obj: Any): Boolean = {
      obj match
        case that: EfficientQueue[T] => (this.front ++ this.rear.reverse) == (that.front ++ that.rear.reverse)
        case _ => false
    }

    override def hashCode(): Int = (front ++ rear.reverse).hashCode()
  }
}
