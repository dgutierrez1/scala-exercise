package co.com.psl.training.scalaintro

import scala.io.Source

object Anagrams {
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically with respect to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** English dictionary. */
  val dictionary: List[Word] =
    Source
      .fromResource(resource = "dictionary.txt")
      .getLines
      .toList
      .map(line => line.toLowerCase)

  // ------------------------------------------------------------------------------------------- //
  // Part 1: Computing Occurrence Lists. ------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** Given a word, produces its occurrence list. */
  def wordOccurrences(w: Word): Occurrences = {
    val res: collection.mutable.Map[Char, Int] = collection.mutable.Map()
    w.toLowerCase
      .toList
      .foreach(c => {
        res(c) = res.getOrElseUpdate(c, 0) + 1
      })
    res.toList
  }


  /** Given a sentence, produces its occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    // wordOccurrences(s.reduce((acc, w) => acc+w))
    wordOccurrences(s.mkString)
  }

  // ------------------------------------------------------------------------------------------- //
  // Part 2: Computing Anagrams of a Word. ----------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** Returns all anagrams of a word, two words are anagrams if they had the same occurrence list. */
  def wordAnagrams(word: Word): List[Word] =
    dictionary.filter(w => wordOccurrences(w) == wordOccurrences(word))
  // I am pretty sure there are better ways of doing this, don't you?
  // ------------------------------------------------------------------------------------------- //
  // Part 3: Computing Anagrams of a Sentence. ------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /**
   *  To compute all the anagrams of a sentence, we will need a helper method which:
   *
   *  Given an occurrence list, produces all the subsets of that occurrence list.
   *
   *  Note: There is only one subset of an empty list, and that is the empty list itself.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    if (occurrences.isEmpty) List(List())
    else {
      val head = occurrences.head
      val combos: List[Occurrences] = combinations(occurrences.tail)
      (for {
        combo <- combos
        n <- 1 to head._2
      } yield (head._1, n) :: combo) ++ combos
    }
  }


  /**
   * We now implement another helper method which:
   *
   * Given two occurrence lists `x` and `y`, subtracts the frequencies of the occurrence list `y`
   * from the frequencies of the occurrence list `x`.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = ???

  /** Returns all anagrams of a sentence. */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???

  // ------------------------------------------------------------------------------------------- //
  // Part 4: Computing Anagrams of a Sentence Optimized (Optional). ---------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** Returns all anagrams of a sentence - (Avoids to recompute values). */
  def sentenceAnagramsMemo(sentence: Sentence): List[Sentence] = ???
}
