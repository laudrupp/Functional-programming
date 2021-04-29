object Exerciseset3 {
  val myName: String = "Lauri Järvisalo"
  val myStudentid: String = "xxxxxxx"
  val solvedExercises: Array[Int] = Array(1,2,3,4,5,6,7,8,9) // for example = Array(1,2,5,6)

  // 1. (1p)
  // Write a recursive function isinstring(c: Char, str: String): Boolean
  // that return true if c appears in str, false if not. Use only string
  // operations isEmpty, head, tail in your implementation.
  // Examples:
  // isinstring('a', "ba") -> true
  // isinstring('a', "") -> false
  def isinstring(c: Char, str: String): Boolean = {
    if(str.isEmpty) return false
    if(str.head == c) true else isinstring(c, str.tail)
  }


  // 2. (2p)
  // Write a recursive function ismin(v: Int, list: List[Int]): Boolean that
  // indicates whether v is the smallest element in list or not (note that v has
  // to be an element in the list). Use only string operations isEmpty, head, tail
  // in your implementation (no contains() etc.).
  // Hint: consider adding a Boolean parameter (with default value) to ismin.
  // (If you add to the function a parameter with a default value it is ok, the
  // function can still be called as specified).
  def ismin(v: Int, list: List[Int]): Boolean = {
    if(!list.isEmpty) {
      if(list.head < v) false else ismin(v, list.tail)
    } else true
  }


  // 3. (1p)
  // Write a recursive function cpos(c: Char, str: String): Int
  // that returns the position (starting from 1) of c in str. If
  // c is not found, return 0. Use only string
  // operations isEmpty, head, tail in your implementation.
  // Hint: consider adding parameter pos with default value
  // to the function.
  // (If you add to the function a parameter with a default value it is ok, the
  // function can still be called as specified).
  def cpos(c: Char, str: String, pos: Int = 1): Int = {
    if(str.isEmpty) return 0
    if(str.head == c) pos else cpos(c, str.tail, pos+1)
  }


  // 4. (1p)
  // Write a function shiftc(c: Char, shift: Int, str: String): Char that
  // returns the character at position of c + shift. Treat str as a ring,
  // ie. the last position is followed by the first position. Use only
  // string operations charAt and length as well as cpos function in the
  // implementation. Note: recursion is not needed.
  def shiftc(c: Char, shift: Int, str: String): Char = {
    var limit: Int = str.length
    var shifted: Int = cpos(c, str) + shift
    if(shifted > limit) shifted = shifted - limit
    str.charAt(shifted-1)
  }


  // 5. (1p)
  // Write a function ccipher(str: String, voca: String): String that
  // returns str Caesar ciphered (each character shifted by 13, possible
  // letters in str are ‘a’...’z’). Use isinstring and shiftc functions
  // and higher-order function map in the implementation. Again, recursion
  // is not needed.
  def ccipher(str: String, voca: String): String = {
    var list = for(i <- 0 until str.length) yield str.charAt(i)
    var temp = list.map(i => shiftc(i, 13, voca))
    temp.mkString
  }


  // 6. (1p)
  // Write function fac(n: Int) that returns the factorial of n (>= 1).
  // Compute the factorial using foldRight. Hint: turn n into a sequence
  // that can be folded.
  def fac(n: Int): Int = {
    if(n > 1) {
      val seq = for(i <- 1 to n) yield i
      seq.foldRight(1)(_ * _)
    } else 1
  }


  // 7. (2p)
  // Write function that returns the minimum of Ints in list as Option[Int].
  // Return value is None for an empty list. Use only list functions isEmpty,
  // head and tail in the implementation.
  // Write the list walkthrough using foldRight. Hint: you might want to
  // consider empty list separately.
  def isminOptional(list: List[Int]): Option[Int] = {
    if(!list.isEmpty) {
      var smallest: Int = list.head
      list.foldRight(0) { (i: Int, acc: Int) =>
        if (i < smallest) {
          smallest = i
        }
        smallest
      }
      Some(smallest)
    } else None
  }


  // 8. (2p)
  // Solve exercise 1 using foldRight, name your function isinstring1.
  // Write the fold statement inside the function.
  def isinstring1(c: Char, str: String): Boolean = {
    str.foldRight(false) {(i, acc) => if(i == c || acc) true else false}
  }


  // 9. (3p)
  // Solve exercise 2 using foldRight. Call the function inside which you have
  // the fold ismin1. Hint: consider using a pair of boolean values as the
  // accumulator in fold. Return value could be a logical combination of the
  // boolean pair returned by fold.
  def ismin1(v: Int, list: List[Int]): Boolean = {
    var answer: Boolean = false
    list.foldRight(false, false) {(i: Int, a: (Boolean, Boolean)) =>
      if(i < v || list.isEmpty && a._2) {
        answer = false
        (false, true)
      } else if(!a._1 && a._2) {
        (false, true)
      } else {
        answer = true
        (true, true)
      }
    }
    answer
  }
}

object Main3 extends App {
  import Exerciseset3._
}