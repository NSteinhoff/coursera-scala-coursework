package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOf2") = forAll { (a: Int, b: Int) =>
    findMin(insert(a, insert(b, empty))) == Math.min(a, b)
  }

  property("deleteMin") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  property("orderedMins") = forAll { (h: H) =>
    val l = toList(h)
    l.init.zip(l.tail).forall{case (x, y) => x <= y}
  }

  property("meldMin") = forAll { (h1: H, h2: H) =>
    val meldMin = findMin(meld(h1, h2))
    val indivMins = List(h1, h2) map findMin
    indivMins contains meldMin
  }

  property("insertTwice") = forAll { (a: Int) =>
    val h = insert(a, insert(a, empty))
    toList(h).length == 2
  }

  property("meldToListLenght") = forAll { (h1: H, h2: H) =>
    toList(meld(h1, h2)).length == (List(h1, h2).map(toList(_).length)).sum
  }

  property("meldToListLenght") = forAll { (h1: H, h2: H) =>
    val inHeaps = List(h1, h2).flatMap(toList)
    val inMeld = toList(meld(h1, h2))
    inHeaps.forall(inMeld contains _)
  }

  def toList(h: H): List[Int] = {
    def inner(rest: H, acc: List[Int]): List[Int] = {
      if (isEmpty(rest)) {
        acc
      } else {
        val e = findMin(rest)
        val new_acc = e :: acc
        inner(deleteMin(rest), new_acc)
      }
    }
    inner(h, List[Int]()).reverse
  }

}
