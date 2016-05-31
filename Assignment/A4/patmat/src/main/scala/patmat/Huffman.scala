package patmat

import common._
//import scala.collection.immutable._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  

  // Part 1: Basics
    def weight(tree: CodeTree): Int = tree match {   // tree match ...
		case (Fork(_,_,_, weight)) => weight
		case (Leaf(_, weight)) => weight
	}
  
    def chars(tree: CodeTree): List[Char] = tree match { // tree match ...
		case (Fork(_,_,chars,_)) => chars
		case (Leaf(char,_)) => List[Char](char)
	}
  
    def makeCodeTree(left: CodeTree, right: CodeTree) =
        Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

	// example of constructing a code tree
	/*
	val sampleTree = makeCodeTree(makeCodeTree(Leaf('x', 1), Leaf('e', 1)), Leaf('t', 2) )
	*/

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
     
    def times(chars: List[Char]): List[(Char, Int)] = {
		// define a function to count each char, input current char and the result list 
		def org(char: Char, count: List[(Char, Int)]) : List[(Char, Int)] = { 
			// if result is empty, add a new pair in
			if (count.isEmpty) count :+ (char, 1)
			// pattern (char, number)
			else count.head match {
				case (c, n) => 
					// char match the first char in count, this pair+1
					if (char == c) (c, n+1) :: count.tail
					// deal with the rest of the pair list and connected with the head
					else count.head :: org(char, count.tail)
			}
		}
		
		// chars is empty: return a empty pair list
		if (chars.isEmpty) List[(Char, Int)]()
		// deal with rest of list first (return is a pair list) and then deal with the first pair
		else org(chars.head, times(chars.tail))
	}

	// version 2 (a simple one)
    /*	
    def times(chars: List[Char]) : List[(Char, Int)] = {
		def org(cs: List[Char], count: List[(Char, Int)]) : List[(Char, Int)] = {
			if (cs.isEmpty) count
			else if (count.isEmpty) org(cs.tail, count:+(cs.head,1))
			else count.head match {
				case (c, n) =>
					if (cs.head == c) org(cs.tail, (c,n+1)::count.tail)
					else org(cs.tail, count.head::org(cs, count.tail))
			}
		}
		org(chars, List[(Char, Int)]())
	}
	*/
  
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
		def makeLeafList(freqs: List[(Char, Int)]) : List[Leaf] = {
			if (freqs.isEmpty) List[Leaf]()
			else Leaf(freqs.head._1, freqs.head._2) :: makeLeafList(freqs.tail)
		}
		makeLeafList(freqs.sortWith((a,b) => a._2 < b._2))
	}
  
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
    def singleton(trees: List[CodeTree]): Boolean = trees.length == 1 
  
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
		if (trees.isEmpty) List[CodeTree]()
		else if (singleton(trees)) trees
		else {
			// get temporary first 2 elements
			val t1 = trees.head
			val t2 = trees.tail.head
			// build a single tree with first 2 elements
			val c = Fork(t1, t2, chars(t1) ::: chars(t2), weight(t1)+weight(t2))
			// sort the resultant list based on weights
			(c :: trees.tail.tail).sortWith((a,b) => weight(a) < weight(b))
		}
	}

  
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(single: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(treeL: List[CodeTree]): List[CodeTree] = 
	  if (single(treeL)) treeL
  	  // update tree list to merge the last two
  	  else until(single, merge)(merge(treeL))
  
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
    // becarefull that the output of until is still a list, need "head"
    def createCodeTree(chars: List[Char]): CodeTree = (until(singleton, combine)(makeOrderedLeafList(times(chars)))).head
  

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
    def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
		// help function
		def dc(t: CodeTree, b: List[Bit]) : List[Char] = t match {
			// leaf -> output char, start a new search from root, bits already minus 1 in the previous level
			case Leaf(char,_) if b.isEmpty => List[Char](char)
			case Leaf(char,_) => char :: dc(tree, b)
			case Fork(left, right, _, _) if b.head == 0 => dc(left, b.tail)
			case Fork(left, right, _, _) => dc(right, b.tail)
		}
		// call help function
		dc(tree, bits)
	}


  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
    def decodedSecret: List[Char] = decode(frenchCode, secret)
  

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = { 
		// a support function for recursion
		def ec(t: CodeTree)(char: Char) : List[Bit] = t match { 
				// case Leaf, no code need to be encoded
				case Leaf(_,_) => List[Bit]()
				// case fork, char in left -> 0, char in right -> 1; recursion
				case Fork(left,right,_,_) =>
					if (chars(left).contains(char)) 0 :: ec(left)(char)
					else                                1 :: ec(right)(char)
		}
		// flatMap ec to each char in text
		text flatMap ec(tree)
	}
			
  
  // Part 4b: Encoding using code table

  // define a new type for pair (char, bit)
  type Code = (Char, List[Bit])
  type CodeTable = List[Code]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
    /*
    def codeBits(table: CodeTable)(char: Char): List[Bit] = 
		if (table.isEmpty) throw new Error("CharNotFound")
		// if char matches the first pair, output the bit of first pair
		else if (char == table.head._1) table.head._2
		// else search the rest
		else codeBits(table.tails)(char)
    */
   def codeBits(table: CodeTable)(char: Char) : List[Bit] =
	   table.filter( (code) => (code._1 == char) ).head._2

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
    def convert(tree: CodeTree): CodeTable = tree match {
		// leaf just output 1 pair with current char and empty bit list in a list
		case Leaf(char,_) => List( (char, List[Bit]() ) )
		// node -> merge the converted left and right, add 0 at beginning for left, and 1 for right
		case Fork(left, right, _, _) => mergeCodeTables(convert(left), convert(right))
  	}

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
		// help function to add bit in front whenever go upper one level, left: add 0; right: add 1
		def coding(bt: Bit)(c: Code) : Code = (c._1, bt :: c._2)
		// map the function (currying) to each pair in the pair list (CodeTable)
		a.map(coding(0)) ::: b.map(coding(1))
	}
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  	// Notice to use FlatMap because the return of codeBits is a List, then if use Map, the final result is List(List)
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = text.flatMap(codeBits(convert(tree)))
  }

	
	// testing
	
	object Main extends App {
		import Huffman._

		val t1 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
		val enc1 = encode(t1)(string2Chars("abd"))
		println( enc1 )
		println("encode finished")
		println( decodedSecret )
		println("decode secret finished")
		println( quickEncode(t1)(string2Chars("abd")) )
		println("quick encode finished")
	}
	

