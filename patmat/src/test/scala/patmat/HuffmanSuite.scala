package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert(times(string2Chars("")) === List())
    assert(times(string2Chars("abcdeabcabaa")) === List(('a', 5), ('b', 3), ('c', 2),('e', 1), ('d', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("it should create a code tree") {
      println(createCodeTree(string2Chars("hdajsdadsdsadasdkaksd")))
  }

  test("decode") {
    new TestTrees {
      assert(decode(t2, List[Bit](0,0,0)) === "a".toList)
      assert(decode(t2, List[Bit](1,0)) === "d".toList)
      assert(decode(t1, List[Bit](0)) === "a".toList)
      assert(decode(t1, List[Bit](1)) === "b".toList)
      assert(decode(t1, List[Bit](0,1)) === "ab".toList)
    }
  }

  test("encode") {
    new TestTrees {
      assert(encode(t2)(string2Chars("a")) === List(0,0))
      assert(encode(t2)(string2Chars("b")) === List(0,1))
      assert(encode(t1)(string2Chars("ab")) === List(0,1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("code bits") {
    assert(codeBits(List[(Char, List[Bit])](('a', List(1,1,1)), ('b', List(0,0,0))))('b') === List(0,0,0))
  }

  test ("code tree") {
      new TestTrees {
        assert(convert(t1) === List[(Char, List[Bit])](('a', List(0)), ('b', List(1))))
      }
  }

  test ("quick encode") {
    new TestTrees {
      assert(quickEncode(t2)(string2Chars("a")) === List(0,0))
      assert(quickEncode(t2)(string2Chars("b")) === List(0,1))
      assert(quickEncode(t1)(string2Chars("ab")) === List(0,1))
    }
  }
}
