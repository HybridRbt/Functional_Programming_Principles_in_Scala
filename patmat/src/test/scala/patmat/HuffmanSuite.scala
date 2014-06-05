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

  test("times(\"hello, world\")") {
    assert(times(string2Chars("hello, world")) === List(('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until function") {
    val leaflist = List(Leaf('g',1),Leaf('s',3),Leaf('a',5))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('g',1),Leaf('s',3),List('g', 's'),4),Leaf('a',5),List('g', 's', 'a'),9)))
  }

  test("decode function 1") {
    val bits: List[Bit] = List(0,1,0,0,1)
    val tree = Fork(Fork(Leaf('A', 1), Leaf('B', 1), List('A', 'B'), 2), Leaf('C', 1), List('A', 'B', 'C'), 3)
    assert(decode(tree, bits) === List('B', 'A', 'C'))
  }

  test("decode function 2") {
    val bits: List[Bit] = List(1,0,0,0,1,0,1,0)
    val tree = Fork(Leaf('A', 8), Fork(Fork(Leaf('B', 3), Fork(Leaf('C', 1), Leaf('D', 1), List('C', 'D'), 2), List('B', 'C', 'D'), 5),
    Fork(Fork(Leaf('E', 1), Leaf('F', 1), List('E', 'F'), 2), Fork(Leaf('G', 1), Leaf('H', 1), List('G', 'H'), 2), List('E', 'F', 'G', 'H'), 4), List('B', 'C', 'D', 'E', 'F', 'G', 'H'), 9)
    , List('A','B', 'C', 'D', 'E', 'F', 'G', 'H'), 17)
    assert(decode(tree, bits) === List('B','A','C'))
  }

  test("decodedSecret function") {
    assert(decodedSecret === List('h','u','f','f','m','a','n','e','s','t','c','o','o','l'))
  }

  test("encode function 1") {
    val text: List[Char] = "BAC".toList
    val tree = Fork(Fork(Leaf('A', 1), Leaf('B', 1), List('A', 'B'), 2), Leaf('C', 1), List('A', 'B', 'C'), 3)
    assert(encode(tree)(text) === List(0,1,0,0,1))
  }

  test("encode function 2") {
    val text: List[Char] = "BAC".toList
    val tree = Fork(Leaf('A', 8), Fork(Fork(Leaf('B', 3), Fork(Leaf('C', 1), Leaf('D', 1), List('C', 'D'), 2), List('B', 'C', 'D'), 5),
      Fork(Fork(Leaf('E', 1), Leaf('F', 1), List('E', 'F'), 2), Fork(Leaf('G', 1), Leaf('H', 1), List('G', 'H'), 2), List('E', 'F', 'G', 'H'), 4), List('B', 'C', 'D', 'E', 'F', 'G', 'H'), 9)
      , List('A','B', 'C', 'D', 'E', 'F', 'G', 'H'), 17)
    assert(encode(tree)(text) === List(1,0,0,0,1,0,1,0))
  }

  test("createCodeTree efficiency") {
    testCodeTreeEfficiency("someText", 22)
    testCodeTreeEfficiency("Huffman est cool", 58)
    testCodeTreeEfficiency("Huffman coding is a compression algorithm that can be used to compress lists of characters.", 373)
  }

  private def testCodeTreeEfficiency(text: String, length: Int) {
    val someTextCodeTree = createCodeTree(text.toList)
    assert(encode(someTextCodeTree)(text.toList).length === length)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
