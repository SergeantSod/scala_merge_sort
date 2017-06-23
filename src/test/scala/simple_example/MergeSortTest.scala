package simple_example

import org.scalactic.TypeCheckedTripleEquals
import simple_example.MergeSort.sort
import org.scalatest._
import prop._

class MergeSortTest extends FunSpec with Matchers with PropertyChecks with TypeCheckedTripleEquals {

  describe("Merge sort"){

    it("leaves an empty sequence untouched"){
      val empty: Seq[Int] = Seq()
      sort(empty) should ===(empty)
    }

    it("leaves a singleton sequence untouched"){
      sort(Seq(1)) should ===(Seq(1))
    }

    it("sorts a small sequence into ascending order"){
      sort(Seq(3,2,1)) should ===(Seq(1,2,3))
    }

    it("leaves a sorted sequence untouched"){
      forAll{ someSequence : Seq[Int] =>
        val sorted = someSequence.sorted
        sort(sorted) should ===(sorted)
      }
    }

    it("arranges the elements into ascending order"){
      forAll{ sortee: Seq[Int] =>
        whenever(sortee.size >= 2) {
          sort(sortee).sliding(2).foreach{ case Seq(left, right) =>
            left should be <= right
          }
        }
      }
    }

    it("preserves all elements"){
      forAll{ sortee: Seq[Int] =>
        sort(sortee) should contain theSameElementsAs(sortee)
      }
    }

  }
}
