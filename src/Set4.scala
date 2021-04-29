object ExerciseSet4 {
    val myName: String = "Lauri Järvisalo"
    val myStudentid: String = "xxxxxxx"
    val solvedExercises: Array[Int] = Array(1,2,3,4,6,7,8,12,13,14,15,16,17) // for example = Array(1,2,5,6)

  object IntList {
    sealed abstract class IntList
    case object EmptyIntList extends IntList
    case class Cons(head: Int, tail: IntList) extends IntList

    def length(list: IntList): Int = list match {
      case EmptyIntList => 0
      case Cons(_, tail) => 1 + length(tail)
    }

    // 1. (1p)
    // Write a recursive function sum(list: IntList): Int that returns the sum of values in list.
    def sum(list: IntList): Int = list match {
      case EmptyIntList => 0
      case Cons(head, tail) => head + sum(tail)
    }

    // 2. (1p)
    // Write a recursive function append(list1: IntList, list2: IntList): IntList that
    // returns a list where list2 is appended to the end of list1.
    def append(list1: IntList, list2: IntList): IntList = list1 match {
      case EmptyIntList => list2
      case Cons(head, EmptyIntList) => Cons(head, list2)
      case Cons(head, tail) => Cons(head, append(tail, list2))
    }

    // 3. (1p)
    // Write a recursive function reverse(list: IntList): IntList that returns a list
    // where elements of the argument list are reversed.
    def reverse(list: IntList, list2: IntList = EmptyIntList): IntList = list match {
      case EmptyIntList => list2
      case Cons(head, tail) => val temp = Cons(head, list2); reverse(tail, temp)
    }

    // 4. (1p)
    // Write a recursive function picknth(n: Int, list: IntList): Option[Int] that returns
    // the nth element from the list or if not possible, None.
    def picknth(n: Int, list: IntList, counter: Int = 1): Option[Int] = list match {
      case EmptyIntList => None
      case Cons(head, tail) => if(n == counter) Some(head) else picknth(n, tail, counter+1)
    }

    // 5. (1p)
    // Write a recursive function filter(list: IntList, cond: Int => Boolean): IntList
    // that returns a list that contains all values in the argument list
    // for which the function cond() returns true.
  }






  object IntBinTree {
    sealed abstract class IntBinTree
    case object EmptyIntBinTree extends IntBinTree
    case class Node(value:Int, left:IntBinTree, right:IntBinTree) extends IntBinTree

    // 6. (1p)
    // Write function count(tree: IntBinTree): Int that returns the number of values in tree.
    def count(tree: IntBinTree): Int = {
      var numberOfValues = 0
      tree match {
        case EmptyIntBinTree => 1
        case Node(_, left, right) => numberOfValues = 1 + count(left) + count(right)
      }
      numberOfValues
    }

    // 7. (1p)
    // Write function height(tree: IntBinTree): Int that returns the maximum number of links
    // between root of the tree and leaves.
    def height(tree: IntBinTree): Int = {
      var numberOfLeft = 0
      var numberOfRight = 0
      var left = tree match {
        case EmptyIntBinTree => 1
        case Node(_, left, right) => numberOfLeft = 1 + height(left)
      }
      var right = tree match {
        case EmptyIntBinTree => 1
        case Node(_, left, right) => numberOfRight = 1 + height(right)
      }
      if(numberOfLeft >= numberOfRight) numberOfLeft else numberOfRight
    }

    // 8. (1p)
    // Write function isIn(tree: IntBinTree, e: Int): Boolean that returns value true if
    // value e is in tree, false otherwise.
    def isIn(tree: IntBinTree, e: Int) : Boolean = tree match {
      case EmptyIntBinTree => false
      case Node(v, left, right) => if(v == e) true else if (v < e) isIn(right, e) else isIn(left, e)
    }

    // 9. (1p)
    // Write function toList(tree: IntBinTree): IntList that returns a list that contains
    // all values in the tree, in sorted order. Note: the elements must be extracted from the
    // tree in sorted order, not sorted afterwards.

    // 10. (1p)
    // Write function fromList(list: IntList): IntBinTree that creates an IntBinTree from values
    // in argument list.

    // 11. (1p)
    // Extra: Exercise 10 but creates a balanced IntBinTree. Hint: you might want to create an
    // ordering that gives a balanced tree.
  }

  object IntStack {
    // 12. (1p)
    // Define IntStack class in the same spirit as IntList and IntBinTree earlier.
    sealed abstract class IntStack
    case object EmptyIntStack extends IntStack
    case class Element(head: Int, tail: IntStack) extends IntStack

    // 13. (1p)
    // push(e: Int, s: IntStack): IntStack - places element e on top of the stack
    def push(e: Int, s: IntStack): IntStack = {
      val x = Element(e, s)
      x
    }

    // 14. (1p)
    // pop(s: IntStack): IntStack - removes the topmost element (calling pop() on
    // empty stack returns empty stack)
    def pop(s: IntStack): IntStack = s match {
      case Element(head, tail) => tail
      case EmptyIntStack => s
    }

    // 15. (1p)
    // top(s: IntStack): Option[Int] - returns the element on top of the stack
    def top(s: IntStack): Option[Int] = s match {
      case Element(head, tail) => Some(head)
      case EmptyIntStack => None
    }

    // 16. (1p)
    // rot(s: IntStack): IntStack (rot() swaps two topmost elements; if stack size is < 2,
    // returns stack unmodified)
    def rot(s: IntStack): IntStack = s match {
      case EmptyIntStack => s
      case Element(head, EmptyIntStack) => s
      case Element(head, tail) => {
        val top1 = top(s)
        var swap = pop(s)
        val top2 = top(swap)
        swap = pop(swap)
        swap = push(top1.getOrElse(head), swap)
        swap = push(top2.getOrElse(head), swap)
        swap
      }
    }
  }

  object Expressions {
    // 17. (3p)
    // Write a case class hierarchy for simple arithmetic expressions. Start with abstract
    // class Expr, and create case classes inheriting from Expr that allow Expr to be:
    //  a Number
    //  two Exprs added: Sum(Expr, Expr)
    //  two Exprs multiplied: Mul(Expr, Expr)
    // Use pattern matching to “parse” the expressions, and to compute their values.
    sealed abstract class Expr
    case class Number(i: Double) extends Expr
    case class Sum(e: Expr, r: Expr) extends Expr
    case class Mul(e: Expr, r: Expr) extends Expr

    def calculate(ex: Expr): Double = ex match {
      case Number(a) => a
      case Sum(a, b) => calculate(a) + calculate(b)
      case Mul(a, b) => calculate(a) * calculate(b)
    }
  }
}

object Main4 extends App {

  import ExerciseSet4.IntList._

  // 1.
  println("Tehtävä 1:")
  val a = Cons(6, Cons(8, Cons(6, EmptyIntList)))
  println(sum(a))
  println("")

  // 2.
  println("Tehtävä 2:")
  val b = Cons(1, Cons(2, Cons(3, EmptyIntList)))
  val c = Cons(4, Cons(5, Cons(6, EmptyIntList)))
  println(append(b, c))
  println("")

  // 3.
  println("Tehtävä 3:")
  val d = Cons(1, Cons(2, Cons(3, EmptyIntList)))
  println(reverse(d))
  println("")

  // 4.
  println("Tehtävä 4:")
  val e = Cons(10, Cons(9, Cons(8, Cons(7, Cons(6, Cons(5, Cons(4, Cons(3, Cons(2, EmptyIntList)))))))))
  println(picknth(9, e))
  println("")

  import ExerciseSet4.IntBinTree._

  // 6.
  println("Tehtävä 6:")
  var f = Node(8, Node(4, Node(5, EmptyIntBinTree, EmptyIntBinTree), EmptyIntBinTree), Node(10, Node(9, EmptyIntBinTree, EmptyIntBinTree), EmptyIntBinTree))
  var g = Node(5, Node(10, EmptyIntBinTree, EmptyIntBinTree), Node(20, EmptyIntBinTree, EmptyIntBinTree))
  var h = Node(5, Node(2, EmptyIntBinTree, EmptyIntBinTree), Node(10, Node(7, Node(6, EmptyIntBinTree, EmptyIntBinTree), EmptyIntBinTree), EmptyIntBinTree))
  println(count(f))
  println("")

  // 7.
  println("Tehtävä 7:")
  println(height(h))
  println("")

  // 8.
  println("Tehtävä 8:")
  println(isIn(f, 10))
  println("")


  import ExerciseSet4.IntStack._

  // 13.
  println("Tehtävä 13:")
  val q = Element(1, Element(2, Element(3, EmptyIntStack)))
  println(push(5, q))
  println("")

  // 14.
  println("Tehtävä 14:")
  println(pop(q))
  println("")

  // 15.
  println("Tehtävä 15:")
  val w = Element(1, Element(2, EmptyIntStack))
  println(top(w))
  println("")

  // 16.
  println("Tehtävä 16:")
  var asd = Element(1, Element(2, EmptyIntStack))
  println(rot(asd))
  println("")

  import ExerciseSet4.Expressions._

  // 17.
  println("Tehtävä 17:")
  println("Example 1: 6+1+2 = " + calculate(Sum(Number(6), Sum(Number(1), Number(2)))))
  println("Example 2: (6+2)*(3+2) = " + calculate(Mul(Sum(Number(6), Number(2)), Sum(Number(3), Number(2)))))
  println("")
}
