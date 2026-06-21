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

def transformList(xs: List[Int], f: Int => Int): List[Int] = xs match
  case Nil          => Nil
  case head :: tail => f(head) :: transformList(tail, f)

// Standalone fold: start from `initialVal`, combine each element with `op`
def reduceListWithOperation(
    xs: List[Int],
    initialVal: Int,
    op: (Int, Int) => Int
): Int = xs match
  case Nil          => initialVal
  case head :: tail => op(head, reduceListWithOperation(tail, initialVal, op))

// Extension methods: lets you call them as list.transform(f) / list.sumList
extension (xs: List[Int])
  def transform(f: Int => Int): List[Int] = xs match
    case Nil          => Nil
    case head :: tail => f(head) :: tail.transform(f)

  def sumList: Int = xs match
    case Nil          => 0
    case head :: tail => head + tail.sumList

  // Keep only the elements that satisfy the predicate (like List.filter)
  def keepOnly(predicate: Int => Boolean): List[Int] = xs match
    case Nil          => Nil
    case head :: tail =>
      if predicate(head) then head :: tail.keepOnly(predicate)
      else tail.keepOnly(predicate)

  // General fold: start from `initialVal`, combine each element with `op`.
  // sumList is reduceWithOperation(0)(_ + _); product is reduceWithOperation(1)(_ * _), etc.
  def reduceWithOperation(initialVal: Int)(op: (Int, Int) => Int): Int =
    xs match
      case Nil          => initialVal
      case head :: tail => op(head, tail.reduceWithOperation(initialVal)(op))

val doubled = listOfInt.transform(_ * 2)
println(s"doubled: $doubled")

// Sum every element -> 15
val total = listOfInt.sumList
println(s"sumList: $total")

// Example: use transform on its own -> List(1, 4, 9, 16, 25)
val squares = listOfInt.transform(x => x * x)
println(s"squares: $squares")

// Example: chain transform then sumList -> sum of squares = 55
val sumOfSquares = listOfInt.transform(x => x * x).sumList
println(s"sumOfSquares: $sumOfSquares")

// Examples: reduceWithOperation — one function, many operations
val sumAll = listOfInt.reduceWithOperation(0)(_ + _) // 1+2+3+4+5  = 15
val product = listOfInt.reduceWithOperation(1)(_ * _) // 1*2*3*4*5  = 120
val maxVal = listOfInt.reduceWithOperation(Int.MinValue)(_ max _) // largest = 5
println(s"sumAll: $sumAll, product: $product, maxVal: $maxVal")

// Combine with transform: product of squares = (1*4*9*16*25) = 14400
val productOfSquares =
  listOfInt.transform(x => x * x).reduceWithOperation(1)(_ * _)
println(s"productOfSquares: $productOfSquares")

// Example: keepOnly even numbers -> List(2, 4)
val evens = listOfInt.keepOnly(x => x % 2 == 0)
println(s"evens: $evens")

// Chain: keep evens, then sum -> 2 + 4 = 6
val sumOfEvens = listOfInt.keepOnly(_ % 2 == 0).sumList
println(s"sumOfEvens: $sumOfEvens")

// Use ALL extensions together: keepOnly -> transform -> reduceWithOperation
// keep odds (1,3,5) -> square them (1,9,25) -> sum -> 35
val pipeline = listOfInt
  .keepOnly(_ % 2 == 1) // List(1, 3, 5)
  .transform(x => x * x) // List(1, 9, 25)
  .reduceWithOperation(0)(_ + _) // 35
println(s"pipeline (filter -> map -> fold): $pipeline")

// Another pipeline: keep numbers > 2, triple them, take the product
// keep (3,4,5) -> triple (9,12,15) -> product -> 1620
val pipeline2 = listOfInt
  .keepOnly(_ > 2) // List(3, 4, 5)
  .transform(_ * 3) // List(9, 12, 15)
  .reduceWithOperation(1)(_ * _) // 1620
println(s"pipeline2 (filter -> map -> fold): $pipeline2")

// Swapping order: keepOnly and transform can be reordered (both List -> List),
// but reduceWithOperation must stay last (it returns an Int, not a List).
// NOTE: the result can change, because the predicate now sees TRIPLED values.

// filter THEN map: keep >2 (3,4,5) -> triple (9,12,15) -> sum -> 36
val filterThenMap = listOfInt
  .keepOnly(_ > 2)
  .transform(_ * 3)
  .reduceWithOperation(0)(_ + _)
println(s"filterThenMap: $filterThenMap")

// map THEN filter: triple all (3,6,9,12,15) -> keep >2 (all) -> sum -> 45
val mapThenFilter = listOfInt
  .transform(_ * 3)
  .keepOnly(_ > 2)
  .reduceWithOperation(0)(_ + _)
println(s"mapThenFilter: $mapThenFilter")

val plusOneList = plusOne(listOfInt)
println(s"plusOneList: $plusOneList")

val multiplyTwoList = multiplyTwo(listOfInt)
println(s"multiplyTwoList: $multiplyTwoList")
