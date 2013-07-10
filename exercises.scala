import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ArrayBuffer
import scala.math.{min, pow}

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
  s.getBytes("UTF8").foldLeft(1)(_*_)
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

// 3.1
def arrayOfN(n: Int) : Array = { (0 until 10) to Array }

// 3.2
def swapInPlace(arr: Array[Int]) {
  // Iterate over only even-indexed elements
  for (i <- 0 until arr.length - 1 if i % 2 == 0) {
    var old = arr(i)
    arr(i) = arr(i + 1)
    arr(i + 1) = old
  }
}

// 3.3
def swapNewArray(arr: Array[Int]) : Array[Int] = {
  val first : Int = (arr.length + 1) / 2
  val sorted : Array[Int] = arr.sortBy(e => arr.indexOf(e) % 2 == 0)
  val left : Array[Int] = sorted.take(first)
  val right : Array[Int] = sorted.takeRight(arr.length - first)
  var buf = ArrayBuffer[Int]()
  for (i <- 0 until (left.length)) {
    buf += left(i)
    try {
      buf += right(i)
    }
  }
  buf.toArray
}

def interleave(arr0: Array[Int], arr1: Array[Int]) : Array[Int] = {
  val shortest : Int = min(arr0.length, arr1.length)
  var buf = ArrayBuffer[Int]()
  for (i <- 0 until shortest) {
    buf += arr1(i)
    buf += arr0(i)
  }
  val longest : List[Array[Int]] = List(arr0, arr1).filterNot(arr =>
    arr.length == shortest)
  if (longest.length > 0) longest(0).takeRight(longest.length - shortest).copyToBuffer(buf)
  buf.toArray
}
