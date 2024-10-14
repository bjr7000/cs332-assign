package forcomp

import common._

import scala.annotation.tailrec

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy(x => x).map{case (c, cl) => c -> cl.length}.toList.sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), Nil)

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    @tailrec
    def appendingCombinationRec(puts: (Char, Int), com: List[Occurrences], acc: List[Occurrences]): List[Occurrences] = {
      com match {
        case Nil => acc
        case head::tail => appendingCombinationRec(puts, tail, (head:+puts)::acc)
      }
    }
    @tailrec
    def appendingCombination(c: Char, times: Int, com: List[Occurrences], acc: List[Occurrences]): List[Occurrences] = {
      if(times == 0) acc
      else appendingCombination(c, times-1, com, appendingCombinationRec((c, times), com, acc))
    }
    @tailrec
    def combinationIter(l: Occurrences, acc: List[Occurrences]): List[Occurrences] = {
      l match {
        case Nil => acc
        case head::tail => combinationIter(tail, appendingCombination(head._1, head._2, acc, acc))
      }
    }
    combinationIter(occurrences, List(Nil))
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   */
  @tailrec
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    @tailrec
    def subtractFind(x: Occurrences, sub: (Char, Int), left: Occurrences): Occurrences = {
      x match {
        case Nil => left
        case (sub._1, t)::tail =>
          if(t > sub._2) left++((sub._1, t-sub._2)::tail)
          else left++tail
        case head::tail => subtractFind(tail, sub, left:+head)
      }
    }
    y match {
      case Nil => x
      case head::tail => subtract(subtractFind(x, head, Nil), tail)
    }
  }

  /** Returns a list of all anagram sentences of the given sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def getAnagrams(occ: Occurrences): List[Sentence] = {
      occ match {
        case Nil => List(Nil)
        case _ => for{
          combination <- combinations(occ)
          validAnagrams <- dictionaryByOccurrences(combination)
          afterUsedFoundAnagram <- getAnagrams(subtract(combination, wordOccurrences(validAnagrams)))
        } yield validAnagrams::afterUsedFoundAnagram
      }
    }
    getAnagrams(sentenceOccurrences(sentence))
  }

}
