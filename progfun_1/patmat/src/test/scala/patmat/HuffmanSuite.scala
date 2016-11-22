package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 =
      Fork(
        Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5),
        Leaf('d',4), List('a','b','d'), 9
      )
    val tree =
      Fork(
        Leaf('a', 1),
        Fork(
          Leaf('b', 2),
          Leaf('c', 4),
          List('b', 'c'),
          6),
        List('a', 'b', 'c'),
        7
      )
    val bits = List(0,1,1,1,0,1,1,0)
    val table = List(
      ('a', List(0)),
      ('b', List(1,0)),
      ('c', List(1,1))
    )
  }

  test("weight of a leaf") {
    assert(weight(Leaf('a', 1)) === 1)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a leaf") {
    assert(chars(Leaf('a', 1)) === List('a'))
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") ===
      List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("times(\"hello, world\")") {
    assert(
      times(string2Chars("hello, world")) ===
        List(
          ('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1),
          ('w', 1), ('r', 1), ('d', 1)),
        "char counts in hello world")
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) ===
      List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("singleton of empty list") {
    assert(singleton(Nil) == false, "Empty list should return false")
  }


  test("singleton of list of one tree") {
    new TestTrees {
      assert(singleton(List(t1)) == true, "Should be singleton")
    }
  }


  test("singleton of list of two trees") {
    new TestTrees {
      assert(singleton(List(t1, t2)) == false, "Should not be singleton")
    }
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) ===
      List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("combine of some leaf list with shuffle") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist) ===
      List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
  }

  test("until(singleton, combine) of some leaf list") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) ===
      Fork(
        Leaf('x',4),
        Fork(
          Leaf('e',2),
          Leaf('t',3),
          List('e', 't'),
          5),
        List('x', 'e', 't'), 9))
  }

  test("createCodeTree(\"abbccc\")") {
    assert(
      createCodeTree(string2Chars("abbcccc")) ===
        Fork(
          Fork(Leaf('a', 1), Leaf('b', 2), List('a','b'), 3),
          Leaf('c', 4),
          List('a', 'b', 'c'), 7))
  }


  test("decode small tree") {
    new TestTrees {
      assert(
        decode(tree, bits) === List('a', 'c', 'b', 'c', 'a')
        )
    }
  }


  test("encode small tree") {
    new TestTrees {
      assert(
        encode(tree)(List('a', 'c', 'b', 'c', 'a')) === bits
        )
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }


  test("codeBits small CodeTable") {
    val table = List(('a', List(1,1,1)), ('b', List(0,1,0)))
    assert(codeBits(table)('a') === List(1,1,1))
    assert(codeBits(table)('b') === List(0,1,0))
  }


  test("convert tree") {
    new TestTrees {
      assert(convert(tree) == table)
    }
  }

  test("quickEncode small tree") {
    new TestTrees {
      assert(
        quickEncode(tree)(List('a', 'c', 'b', 'c', 'a')) === bits
        )
    }
  }


}
