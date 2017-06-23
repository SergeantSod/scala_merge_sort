package simple_example

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.math.Ordering.Implicits._

import scala.util.control.TailCalls._

object MergeSort {

  /*
    A relatively light-weight handle for a sub-sequence of an Array that provides operations that are useful for
    MergeSort without the need for copying (parts of) the Array.
   */
  class ArraySlice[E: ClassTag](val start: Int, val length: Int, val array: Array[E]) {

    require(start >= 0, "start too small")
    require(length >= 0, "negative length")
    require(start <= array.length, "start beyond array length")
    require(start + length <= array.length, "length beyond array length")

    def this(length: Int) = {
      this(0, length, new Array[E](length))
    }

    def this(array: Array[E]) = {
      this(0, array.length, array)
    }

    /*
      Returns a pair of ArraySlices, each of which covers half of the current ArraySlice. Both are backed by the
      receiver's underlying Array.
     */
    def half(): (ArraySlice[E], ArraySlice[E]) = {
      checkEmpty()

      val newLeftLength = length / 2

      val left = new ArraySlice[E](start, newLeftLength, array)
      val right = new ArraySlice[E](start + newLeftLength, length - newLeftLength, array)
      (left, right)
    }

    /*
      Replace the array element at the head end of the array slice with another one (in-place) and return the receiver's tail.
     */
    def advance(newHead: E): ArraySlice[E] = {
      array(start) = newHead
      tail
    }

    /*
      Returns the tail of this ArraySlice (i.e. everything except the first element) as an ArraySlice.
     */
    def tail: ArraySlice[E] = {
      checkEmpty()
      new ArraySlice[E](start + 1, length - 1, array)
    }

    /*
      Retrieves the first element of the ArraySlice.
     */
    def head: E = {
      checkEmpty()
      array(start)
    }

    private def checkEmpty(): Unit = {
      if (isEmpty()) throw new NoSuchElementException
    }


    def isEmpty(): Boolean = {
      length <= 0
    }

    override def toString: String = {
      s"ArrayWindow($elementsToString)"
    }

    private def elementsToString: String = {
      if (isEmpty()) {
        ""
      }
      else {
        head + ", " + tail.elementsToString
      }
    }

  }

  object ArraySlice {
    def copyOf[E: ClassTag](clonee: Array[E]): ArraySlice[E] = {
      new ArraySlice[E](clonee.clone())
    }
  }


  def sort[E: ClassTag : Ordering](sortee: Seq[E]): Seq[E] = sort(sortee.toArray).toSeq

  def sort[E: ClassTag : Ordering](sortee: Array[E]): Array[E] = {
    val sortTarget = new ArraySlice[E](sortee)
    val sortBuffer = ArraySlice.copyOf(sortee)
    sort(sortTarget, sortBuffer).result
    sortTarget.array
  }

  private def sort[E: ClassTag : Ordering](sortee: ArraySlice[E], buffer: ArraySlice[E]): TailRec[Unit] = {
    /*
      invariants:
        - buffer is a copy of sortee at invocation
        - buffer and copy are not backed by the same physical array
       output:
        - sortee is sorted in place

      The tricky bit is that merge needs to go from one buffer to the other, since in-place merging is REALLY hard. The
      second buffer was introduced to be able to avoid this in the first place.

      Since sort is in-place and merge should be the last step without any additional copying, it follows that the merge
      is from the buffer back into sortee. The merge is from two internally sorted sequences. So we sorted the the two
      parts of the buffer in-place. This is why the first recursion invariant is necessary: It guarantees that we are
      operating on the same elements (without needing an additional copy step at some point). For this recursive part we
      need to pass a buffer space that satisfies the invariant. To not need further allocation, we pass the original
      sortee for this.
    */

    // if the buffer is empty or a singleton, it's already sorted, so there's nothing to do.
    // this also takes care of the base case of the recursion.
    if (buffer.length >= 2) {
      //divide buffers into parts
      val (leftBuffer, rightBuffer) = buffer.half()
      val (leftSortee, rightSortee) = sortee.half()

      for {
        //sort parts recursively in-place
        _ <- sort(leftBuffer, leftSortee)
        _ <- sort(rightBuffer, rightSortee)
      } yield mergeInto(leftBuffer, rightBuffer)(sortee)
    } else {
      done(())
    }
  }

  @tailrec
  def mergeInto[E: ClassTag : Ordering](left: ArraySlice[E], right: ArraySlice[E])(target: ArraySlice[E]): Unit = {
    if (!right.isEmpty() && (left.isEmpty() || right.head <= left.head)) {
      mergeInto(left, right.tail)(target.advance(right.head))
    } else if (!left.isEmpty() && (right.isEmpty() || left.head <= right.head)) {
      mergeInto(left.tail, right)(target.advance(left.head))
    }
    //otherwise both sources are empty and we're done
  }

}
