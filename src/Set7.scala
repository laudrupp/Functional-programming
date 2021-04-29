import ExerciseSet7Recap._

object ExerciseSet7Recap {
  val myName: String = "Lauri Järvisalo"
  val myStudentid: String = "xxxxxxx"
  val solvedExercises: Array[Int] = Array(1,4,5,6,7,8) // for example = Array(1,2,5,6)

  sealed abstract class IntList
  case object EmptyIntList extends IntList
  case class Cons(head: Int, tail: IntList) extends IntList

  // Exercise 1 (1p): Write function intListSorted that returns true if the argument
  // list is sorted, false otherwise.
  def intListSorted(xs: IntList, x: Int = 0): Boolean = xs match {
      case EmptyIntList => true
      case Cons(head, tail) => if(head < x) false else intListSorted(tail, head)
  }

  // Exercise 2 (1p): Write exercise 1 function using fold.

  // Exercise 4 (1p): Write a case class definition for IntSet. Hint: IntList but make the constructor
  // that grows the set private so that you can make sure set has no duplicates. Write your case class
  // inside companion object and write the functions in exercises 5-10 in the companion object.
  sealed abstract class IntSet
  case object Empty extends IntSet
  case class NonEmpty private (head: Int, tail: IntSet) extends IntSet

  // Exercise 5 (1p): Write function isIn(e: Int, s: IntSet): Boolean that returns true if e is in s,
  // false otherwise
  def isIn(e: Int, s:IntSet): Boolean = s match {
    case Empty => false
    case NonEmpty(head, tail) => if(head == e) true else isIn(e, tail)
  }

  // Exercise 6 (1p): Write function add(e: Int, s: IntSet): IntSet that adds element e to s. Make sure
  // you don't create duplicates.
  def add(e: Int, s: IntSet): IntSet = s match {
    case Empty => NonEmpty(e, Empty)
    case NonEmpty(head, tail) => NonEmpty(e, s)
  }

  // Exercise 7 (1p): Write function toIntList(xs: IntSet): IntList
  def toIntList(xs: IntSet): IntList = xs match {
    case Empty => EmptyIntList
    case NonEmpty(head, tail) => Cons(head, toIntList(tail))
  }

  // Exercise 8 (1p): Write function toIntSet(xs: IntList): IntSet
  def toIntSet(xs: IntList): IntSet = xs match {
    case EmptyIntList => Empty
    case Cons(head, tail) => NonEmpty(head, toIntSet(tail))
  }

  // Exercise 9 (1p): Write function intersect(xs1: IntSet, xs2: IntSet): IntSet that returns set with
  // elements that are both in xs1 and xs2.

  // Exercise 10 (1p): Write function union(xs1: IntSet, xs2: IntSet): IntSet that returns set with
  // elements that are either in xs1 or in xs2. Make sure you don't create duplicates.

}

object Main7 extends App {

  // 1.
  println("1:")
  var a = Cons(1, Cons(2, Cons(3, Cons(4, EmptyIntList))))
  println(intListSorted(a))
  var b = Cons(2, Cons(1, Cons(10, EmptyIntList)))
  println(intListSorted(b))
  println("")

  // 5.
  println("5.")
  var c = NonEmpty(1, NonEmpty(2, NonEmpty(3, Empty)))
  println(isIn(1, c))
  println(isIn(4, c))
  println("")

  // 6.
  println("6.")
  println(add(20, c))
  println("")

  // 7.
  println("7.")
  println(toIntList(c))
  println("")

  // 8.
  println("8.")
  println(toIntSet(a))
  println("")

}
