import scala.collection.immutable.IndexedSeq
import scala.math.pow

// 2.1
def getSignum(n: Int) : Int = {
  if (n > 0) 1 else if (n < 0) -1 else 0
}

// 2.2
// In `var y = {}`, y is type Unit and value is null.

// 2.3
{var x : Unit = y = 1}

// 2.4
def loopEquiv() : Unit = {
  for (i <- 1 to 10) print(i)
}

// ...or...
def loopEquiv() {
  // No return value, so type is implicitly `Unit`
  for (i <- 1 to 10) print(i)
}

// 2.5
def countdown(n: Int) {
  if (n <= 0) return print(0)
  var r = n
  while (r >= 0) {
    printf("%s\n", r)
    r -= 1
  }
}

// 2.6
def unicodeProduct(s: String) : Int = {
  var start : Int = 1
  for (i <- s) {
    start *= i.toByte
  }
  start
}

// 2.7
def unicodeProductNoLoop(s: String) : Int = {
  val s2 : IndexedSeq[Byte] = s map (c => c.toByte)
  s2.foldLeft(1) { (total, n) =>
    total * n
  }
}

// 2.8
// Covered above

// 2.9
def recurProduct(s: String, start: Int = 1) : Int = {
  if (s.length == 1) return s.head.toByte * start
  recurProduct(s.tail, s.head.toByte * start)
}

// 2.10
// This one is kind of dumb, just turns out to be silly algebra gotcha.
def xToThe(x: Double, n: Int) : Double = {
  pow(x, n)
}
