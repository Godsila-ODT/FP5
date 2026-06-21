// A custom "Maybe" type built from scratch (Scala's Option under the hood).
// Just(value) = success, Empty = no value.
// The trait declares WHAT each operation is; each case implements HOW.
sealed trait Maybe[+A]:
  def map[B](f: A => B): Maybe[B]
  def flatMap[B](f: A => Maybe[B]): Maybe[B]
  def getOrElse[B >: A](default: => B): B
  // fold: collapse to a B — run ifEmpty when Empty, else apply f to the value.
  def fold[B](ifEmpty: => B)(f: A => B): B

case class Just[+A](value: A) extends Maybe[A]:
  def map[B](f: A => B): Maybe[B] = Just(f(value))
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = f(value)
  def getOrElse[B >: A](default: => B): B = value
  def fold[B](ifEmpty: => B)(f: A => B): B = f(value)

case object Empty extends Maybe[Nothing]:
  def map[B](f: Nothing => B): Maybe[B] = Empty
  def flatMap[B](f: Nothing => Maybe[B]): Maybe[B] = Empty
  def getOrElse[B](default: => B): B = default
  def fold[B](ifEmpty: => B)(f: Nothing => B): B = ifEmpty

// Safe divide: returns Empty instead of crashing on divide-by-zero.
def divide(a: Int, b: Int): Maybe[Int] =
  if b == 0 then Empty
  else Just(a / b)

// Try them out:
val ok = divide(10, 2) // Just(5)
val bad = divide(10, 0) // Empty
println(s"divide(10, 2) = $ok")
println(s"divide(10, 0) = $bad")

// map: transform the value inside, only if it's present.
val doubled = divide(10, 2).map(v => v * 2)
val divid = divide(10, 2)
val doub = divid.map(v => v * 2)
println(s"map (Just) = $doubled") // Just(10)

// On Empty, map does nothing — stays Empty, no crash:
val doubledFail = divide(10, 0).map(v => v * 2)
println(s"map (Empty) = $doubledFail") // Empty

// Chaining: only continues while each step is Just; any Empty short-circuits.
// (10 / 2) / 5 = 1
val chained = divide(10, 2).flatMap(r => divide(r, 5))
println(s"chained = $chained") // Just(1)

// An Empty anywhere stops the chain — no crash:
val chainedFail = divide(10, 0).flatMap(r => divide(r, 5))
println(s"chainedFail = $chainedFail") // Empty

// Provide a fallback when there's no value:
val safe = divide(10, 0).getOrElse(-1)
println(s"divide(10, 0).getOrElse(-1) = $safe") // -1

// fold: handle both cases at once, collapsing the Maybe into a plain value.
val report = divide(10, 2).fold("no result")(v => s"got $v")
println(s"fold (Just) = $report") // got 5

val reportFail = divide(10, 0).fold("no result")(v => s"got $v")
println(s"fold (Empty) = $reportFail") // no result
