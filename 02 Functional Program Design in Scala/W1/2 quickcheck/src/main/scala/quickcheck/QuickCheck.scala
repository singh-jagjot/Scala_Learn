package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(const(empty),
    for
      x <- arbitrary[A]
      y <- oneOf(const(empty), genHeap)
    yield insert(x, y)
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //  If you insert any two elements into an empty heap, finding the minimum of the resulting heap
  //  should get the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    val min = if a < b then a else b
    val h = insert(a, insert(b, empty))
    findMin(h) == min
  }

  //  If you insert an element into an empty heap, then delete the minimum,
  //  the resulting heap should be empty.
  property("empty1") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    property("min3") = forAll { (h1: H, h2: H) =>
      if !isEmpty(h1) && !isEmpty(h2) then
        val h1min = findMin(h1)
        val h2min = findMin(h2)
        val min = if h1min < h2min then h1min else h2min
        findMin(meld(h1, h2)) == min
      else if isEmpty(h1) then findMin(meld(h1, h2)) == findMin(h2)
      else if isEmpty(h2) then findMin(meld(h1, h2)) == findMin(h1)
      else true
    }

  //  Given any heap, you should get a sorted sequence of elements when continually finding and
  //  deleting minima. (Hint: recursion and helper functions are your friends.)
  property("sorted") = forAll { (h: H) => isSorted(h) }

  def isSorted(h: H, l: List[A] = List()): Boolean =
    if isEmpty(h) then l == l.sorted.reverse else isSorted(deleteMin(h), findMin(h) :: l)
