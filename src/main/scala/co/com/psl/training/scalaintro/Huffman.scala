package co.com.psl.training.scalaintro

object Huffman  extends App {

  /** Represents a Huffman tree. */
  sealed trait CodeTree extends Product with Serializable

  final case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  final case class Leaf(char: Char, weight: Int) extends CodeTree

  /** For simplicity, we will use Boolean for representing Bits. true = 1   &   false = 0 */
  type Bit = Boolean

  // ------------------------------------------------------------------------------------------- //
  // Part 1: Basics. --------------------------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** Returns the weight of Huffman tree. */
  def weight(tree: CodeTree): Int = {
    if (tree.isInstanceOf[Leaf]) {
      return tree.asInstanceOf[Leaf].weight
    } else if (tree.isInstanceOf[Fork]) {
      val fork = tree.asInstanceOf[Fork]
      return weight(fork.left) + weight(fork.right)
    }
    0
  }

  /** Returns the list of characters defined in a given Huffman tree. */
  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(ch, _ ) => List(ch)
    case Fork(left, right, _, _) => chars(left) ::: chars(right)
  }

  /** Returns the union of two Huffman trees. */
  def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree = {
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
  }


  // ------------------------------------------------------------------------------------------- //
  // Part 2: Generating Huffman trees. --------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** Returns the optimal Huffman tree from a String (List of characters). */
  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine, makeOrderedLeafList(times(chars))).head
  }

  /**
    * This function computes for each unique character in the list `chars` the number of
    * times it occurs. For example, the invocation
    *
    * times(List('a', 'b', 'a'))
    *
    * should return the following (the order of the resulting list is not important):
    *
    * List(('a', 2), ('b', 1))
    */
  def times(chars: List[Char]): List[(Char, Int)] =
    chars
      .groupBy(identity)
      .map(c => (c._1, c._2.length))
      .toList

  /**
    * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights
    * (i.e. the head of the list should have the smallest weight),
    * where the weight of a leaf is the frequency of the character.
    */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs
      .map(freq => Leaf(freq._1, freq._2))
      .sortWith((lf1, lf2) => weight(lf1) < weight(lf2))
  }


  /** Checks whether the list `trees` contains only one single code tree. */
  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  /** Inserts an element in the right place of a sorted ascending list. */
  def insert(elem: CodeTree, list: List[CodeTree]): List[CodeTree] = {
    val partitions = list.partition(weight(_) > weight(elem))
    partitions._1 ::: elem :: partitions._2
  }

  /**
    * The parameter `trees` of this function is a list of code trees ordered
    * by ascending weights.
    *
    * This function takes the first two elements of the list `trees` and combines
    * them into a single `Fork` node. This node is then added back into the
    * remaining elements of `trees` at a position such that the ordering by weights
    * is preserved.
    *
    * If `trees` is a list of less than two elements, that list should be returned
    * unchanged.
    */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    var codeTree = trees
    val first = codeTree.headOption.get
    codeTree = codeTree.drop(1)
    val second = codeTree.headOption.get
    codeTree =  codeTree.drop(1)
    val fork = makeCodeTree(first, second)
    insert(fork, codeTree)
  }

  /** Continuously applies a transformation to a List, until some condition is met. */
  def until(condition: List[CodeTree] => Boolean, transformation: (List[CodeTree]) => List[CodeTree], trees: List[CodeTree]): List[CodeTree] = {
    if (condition(trees)) trees
    else until(condition, transformation, transformation(trees))
  }

  // Do yourself a favor, and first fix the type signature!

  // ------------------------------------------------------------------------------------------- //
  // Part 3: Encoding. ------------------------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //
  //   Part A: Using a Huffman Tree. ----------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** This function encodes text, using the code tree, into a sequence of bits. */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encoder(t: CodeTree, ch: Char): List[Bit] = t match {
      case Leaf(_, _) => List()
      case Fork(l, r, _, _) => if (chars(l).contains(ch)) false :: encoder(l, ch) else true :: encoder(r, ch)
    }
    text.flatMap(ch => encoder(tree, ch))
  }

  // ------------------------------------------------------------------------------------------- //
  //   Part B: Using a Coding Table (Optional). ------------------------------------------------ //
  // ------------------------------------------------------------------------------------------- //

  /**
    * The previous function is simple, but very inefficient.
    * You goal is now to define quickEncode which encodes an equivalent representation,
    * but more efficiently. By building a CodeTable once, and accessing it latter.
    */
  type CodeTable = List[(Char, List[Bit])]

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    */
  def convert(tree: CodeTree): CodeTable =
    tree match {
      case Leaf(c, _) => List((c, List()))
      case Fork(left, right, _, _) => joinCodeTables(convert(left), convert(right))
    }

  def joinCodeTables(ct1: CodeTable, ct2: CodeTable): CodeTable = {
    def mapBit(bit: Bit)(codeTable: CodeTable): CodeTable = codeTable.map(code => (code._1, bit :: code._2))

    mapBit(false)(ct1) ::: mapBit(true)(ct2)
  }

  /** This function encodes text, using a code tree, into a sequence of bits. */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] =
    text.flatMap(ch => convert(tree).find(c => c._1 == ch).get._2)

  // ------------------------------------------------------------------------------------------- //
  // Part 4: Decoding. ------------------------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** This function decodes the bit sequence, using a code tree, into a text. */
  def decode(tree: CodeTree)(bits: List[Bit]): List[Char] = {
    def decodeHelper(t: CodeTree, b: List[Bit]): List[Char] = t match {
      case Leaf(ch, _) => if (b.isEmpty) List(ch) else ch :: decodeHelper(tree, b)
      case Fork(left, right, _, _) => if (b.head == false) decodeHelper(left, b.tail) else decodeHelper(right, b.tail)
    }
    decodeHelper(tree, bits)
  }

}
