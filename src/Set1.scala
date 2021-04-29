
object ExerciseSet1 {
  val myName: String = "Lauri Järvisalo"
  val myStudentid: String = "xxxxxxx"
  val solvedExercises: Array[Int] = Array(1,2,3,4,5,6) // for example = Array(1,2,5,6)

  import scala.util.Random._

  // 1. (1p) Write the expression (3+5) * (2*6 + 1) / 2 using Int method call syntax
  // (for example 1.+(2)) and assign it to val ans1
  val ans1: Double = 3.+(5).*(2.*(6).+(1))./(2)

  // 2. (1p) Write a function pyth() that returns a Vector of tuples (a,b,c) of Ints such
  // that a <= limit, b <= limit, c <= limit and a,b,c are lengths of sides of right triangle
  // (c is the hypotenuse).
  // Use advanced for and yield - you will get the right kind of collection as result.
  // Hint: a, b and c must fulfill the Pythagoras identity.
  // Example: pyth(5) -> Vector((3,4,5), (4,3,5))
  def pyth(limit: Int) = for(a <- 1 to limit; b <- 1 to limit; c <- 1 to limit if (a*a)+(b*b) == (c*c)) yield (a,b,c)

  // 3. (1p) Write functions pow_loop() and pow_rec() that return their parameter b raised to power p.
  // In pow_loop(), use iteration, or loop, and in pow_rec() use recursion.
  def pow_loop(b: Int, p: Int): Int = {
    var product = 1
    for (i <- 1 to p) product = product*b
    product
  }
  def pow_rec(b: Int, p: Int): Int = if(p == 0) 1 else b * pow_rec(b, p-1)

  // 4. (1p) Write function myShuffle() that shuffles its argument Array[Int] by looping through all
  // array positions i from 0 to last and returns a new shuffled array. At each round of the loop,
  // swap the element with a random element in the array (if parameter swapAll is true, use
  // Random.between(0, theArray.size), otherwise Random.between(i, theArray.size) where i is the
  // loop index variable).
  def myShuffle(numbers: Array[Int], swapAll: Boolean): Array[Int] = {
    val rnd = new scala.util.Random
    if(swapAll == true) {
      for (i <- 0 to numbers.length-1) {
        var x = rnd.between(0, numbers.size)
        var y = numbers(i)
        numbers(i) = numbers(x)
        numbers(x) = y
      }
    } else {
      for(i <- 0 to numbers.length-1) {
        var x = rnd.between(i, numbers.size)
        var y = numbers(i)
        numbers(i) = numbers(x)
        numbers(x) = y
      }
    }
    numbers
  }

  // 5. (1p) Write function mean that returns the mean of elements in its parameter array.
  // Write also function stdev that returns the sample standard deviation of the elements in its
  // parameter array. Use your mean() function in the implementation. stdev is calculated by
  // computing the sum of (m - e(i))^2, divide it with N-1 and take square root. Here m is
  // the mean, e(i) the ith element and N the number of elements. See for example
  // https://www.mathsisfun.com/data/standard-deviation-formulas.html for a detailed explanation
  // (scroll down for sample standard deviation). Hint: use yield to get the squares.
  def mean(a: Array[Double]): Double = {
    var sum = 0.0
    for(i <- 0 to a.length - 1) sum += a(i)
    var answer = sum./(a.length)
    answer
  }
  def stdev(a: Array[Double]): Double = {
    var m = mean(a)
    var sum = 0.0
    val squares = for(element <- a) yield (element - m) * (element - m)
    for (element <- squares) sum += element
    var variance = sum./(squares.length-1)
    var answer = math.sqrt(variance)
    answer
  }

  // 6. (1p) Write function meansForPositions() that shuffles (use your shuffle function) numbers
  // in range 1..upper (upper included) for iters times and calculates means for each position in range.
  def meansForPositions(upper: Int, iters: Int, swapAll: Boolean): Array[Double] = {
    val a = new Array[Int](upper)
    val temp = new Array[Int](upper)
    for(i <- 0 until upper) a(i) = (i+1)
    for(j <- 0 until iters) {
      myShuffle(a, swapAll)
      for(k <- 0 until upper) temp(k) += a(k)
    }
    val arr = new Array[Double](upper)
    for(l <- 0 until upper) arr(l) = (temp(l)./(iters)).toDouble
    arr
  }

  // 7. (1p) Using functions you wrote earlier, compare the position means for the two different
  // shuffling algorithm variants (swapAll = true/false). Compute the stdev for both. Which one
  // has lower stdev and gives more uniform result?
  val swapAllIsBetter: Boolean = ???

  // 8. (1p) Write a function numbers(range, n) with one-line expression that can be used to generate
  // n unique numbers from range in sorted order. You can assume range contains at least n numbers.
  // Hint: a range can be converted to a list, which can be shuffled, and some numbers taken from
  // that. Use shuffle() in scala.util.Random for shuffling.
  def numbers(range: Range, n: Int) = ???
}

object Main extends App {
  import ExerciseSet1._
  // Here you can write code to try out your solutions.
  // This part is not graded.
  println(myName)
  println(myStudentid)
  println(solvedExercises)
}
