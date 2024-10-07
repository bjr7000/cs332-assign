package patmat

import common._

import scala.annotation.tailrec

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, w) => w
    case Leaf(_, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, c, _) => c
    case Leaf(c, _) => c::List()
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def listIncl(c: Char, l:List[(Char, Int)]): List[(Char, Int)] = {
      @tailrec
      def listInclTail(c: Char, l:List[(Char, Int)], acc:List[(Char, Int)]): List[(Char, Int)] = {
        l match {
          case List() => acc
          case (ch, v)::tail =>
            if(ch == c) listInclTail(c, tail, (ch, v+1)::acc)
            else listInclTail(c, tail, (ch, v)::acc)
        }
      }
      listInclTail(c, l, List())
    }
    @tailrec
    def timesRec(chars: List[Char], times: List[(Char, Int)]): List[(Char, Int)] = {
      chars match {
        case List() => times
        case head::tail => timesRec(tail, listIncl(head, times))
      }
    }
    timesRec(chars, List())
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def insertOrder(freq: (Char, Int), leaves: List[Leaf]): List[Leaf] = {
      leaves match {
        case List() => List(Leaf(freq._1, freq._2))
        case head::tail =>
          if(freq._1 > head.weight) head::insertOrder(freq, tail)
          else Leaf(freq._1, freq._2)::leaves
      }
    }
    @tailrec
    def makeOrderedLeafListTail(freqs: List[(Char, Int)], acc: List[Leaf]): List[Leaf] = {
      freqs match {
        case List() => acc
        case head::tail => makeOrderedLeafListTail(tail, insertOrder(head, acc))
      }
    }
    makeOrderedLeafListTail(freqs, List())
  }

  def singleton(trees: List[CodeTree]): Boolean = trees.size == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    trees match {
      case head::mid::tail => makeCodeTree(head, mid)::tail
      case _ => trees
    }
  }

  @tailrec
  def util(singleton: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): CodeTree = {
    if(singleton(trees)) trees.head
    else util(singleton, combine)(combine(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = util(singleton, combine)(makeOrderedLeafList(times(chars)))


  // Part 3: Decoding

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    @tailrec
    def decodeTail(partTree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = {
      partTree match {
        case Leaf(c, _) => decodeTail(tree, bits, acc:+c)
        case Fork(l, r, _, _) => bits match {
          case List() => acc
          case 0::tail => decodeTail(l, tail, acc)
          case 1::tail => decodeTail(r, tail, acc)
          case _::tail => throw new NumberFormatException("decode: bits should contain only 0 and 1")
        }
      }
    }
    decodeTail(tree, bits, List())
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    @tailrec
    def contains(c: Char, t: List[Char]): Boolean = {
      t match {
        case List() => false
        case head::tail => if(c == head) true else contains(c, tail)
      }
    }
    @tailrec
    def encodeTail(partTree: CodeTree, text: List[Char], acc: List[Bit]): List[Bit] = {
      text match {
        case List() => acc
        case head::tail => partTree match {
          case Leaf(_, _) => encodeTail(tree, tail, acc)
          case Fork(l, r, _, _) =>
            if(contains(head, chars(l))) encodeTail(l, text, acc:+0)
            else if(contains(head, chars(r))) encodeTail(r, text, acc:+1)
            else throw new NoSuchElementException("encode: there is a char which is not member of part of tree")
        }
      }
    }
    encodeTail(tree, text, List())
  }


  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  @tailrec
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table match {
      case List() => throw new NoSuchElementException("codeBits: char does not exist in the table")
      case head::tail => if(head._1 == char) head._2 else codeBits(tail)(char)
    }
  }

  def convert(tree: CodeTree): CodeTable = {
    def convertRecur(tree: CodeTree, bits: List[Bit]): CodeTable = {
      tree match {
        case Leaf(c, _) => List((c, bits))
        case Fork(l, r, _, _) => mergeCodeTables(convertRecur(l, bits:+0), convertRecur(r, bits:+1))
      }
    }
    convertRecur(tree, List())
  }

  @tailrec
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a match {
      case List() => b
      case head::tail => mergeCodeTables(tail, head::b)
    }
  }

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    @tailrec
    def quickEncodeTail(table: CodeTable, text: List[Char], acc: List[Bit]): List[Bit] = {
      text match {
        case List() => acc
        case head::tail => quickEncodeTail(table, tail, codeBits(table)(head):::acc)
      }
    }
    quickEncodeTail(convert(tree), text, List())
  }
}
