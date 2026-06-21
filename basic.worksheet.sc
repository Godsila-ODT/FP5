println("Hello, World!")

// Fibonacci — simple recursion (clear, but exponential time)
def fib(n: Int): Int =
  if n < 2 then n
  else fib(n - 1) + fib(n - 2)

// Fibonacci — tail recursive (linear time, stack-safe via @tailrec)
import scala.annotation.tailrec

def fibTailRec(n: Int): Int =
  @tailrec
  def loop(i: Int, prev: Int, curr: Int): Int =
    if i == n then prev
    else loop(i + 1, curr, prev + curr)
  loop(0, 0, 1)

// Factorial — simple recursion
def factorial(n: Int): BigInt =
  if n <= 1 then 1
  else n * factorial(n - 1)

// Factorial — tail recursive
def factorialTailRec(n: Int): BigInt =
  @tailrec
  def loop(i: Int, acc: BigInt): BigInt =
    if i <= 1 then acc
    else loop(i - 1, acc * i)
  loop(n, 1)

// Try them out:
val first10 = (0 to 9).map(fib).toList
println(s"fib:        $first10")

val first10TR = (0 to 9).map(fibTailRec).toList
println(s"fibTailRec: $first10TR")

val facts = (0 to 10).map(factorial).toList
println(s"factorial:  $facts")

val listOfInt = List(1, 2, 3, 4, 5)

// Add 1 to every element -> List(2, 3, 4, 5, 6)
def plusOne(xs: List[Int]): List[Int] = xs match
  case Nil          => Nil
  case head :: tail => (head + 1) :: plusOne(tail)

// Multiply 2 to every element -> List(2, 4, 6, 8, 10)
def multiplyTwo(xs: List[Int]): List[Int] = xs match
  case Nil          => Nil
  case head :: tail => (head * 2) :: multiplyTwo(tail)

val plusOneList = plusOne(listOfInt)
println(s"plusOneList: $plusOneList")

val multiplyTwoList = multiplyTwo(listOfInt)
println(s"multiplyTwoList: $multiplyTwoList")
