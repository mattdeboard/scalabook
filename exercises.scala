import scala.collection.immutable.SortedMap
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
def loopEquivAlt() {
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
def arrayOfN(n: Int) : Array = { (0 until n) to Array }

// 3.2
def swapInPlace(arr: Array[Int]) {
  // Iterate over only even-indexed elements
  for (i <- 0 until arr.length - 1 if i % 2 == 0) {
    val old = arr(i)
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
  for (i <- 0 until left.length) {
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

// 4.1
{
  val prices = Map("car" -> 10000, "boat" -> 15000, "lamp" -> 150)
  for ((k, v) <- prices) yield Map(k -> v*.9)
}

// 4.2
def wordCount(path: String) : scala.collection.mutable.Map[String, Int] = {
  val counts = scala.collection.mutable.Map[String, Int]()
  val in = new java.util.Scanner(new java.io.FileReader(path))
  while (in.hasNext) {
    val word = in.next
    counts(word) = if (counts.contains(word)) counts(word) + 1 else 1
  }
  counts
}

// 4.3
def wordCountImmutable(path: String) : Map[String, Int] = {
  // no clue.
}

// 4.4
def wordCountSorted(path: String) : SortedMap[String, Int] = {
  // fuck this. use mutable types.
  SortedMap()
}

// 5.2
class BankAccount(private var balance: Double = 0.0) {
  def transaction(amt: Double, transType: String = "deposit") {
    this.balance = transType match {
      case "deposit" => this.balance + amt
      case "withdraw" => this.balance - amt
    }
    printf("New balance is $%.2f", this.balance)
  }

  def deposit(amt: Double) {
    this.transaction(amt, "deposit")
  }

  def withdraw(amt: Double) {
    this.transaction(amt, "withdraw")
  }
}

// 5.3
// Also did 5.4 inadvertently.
class Time(val hours: Int, val minutes: Int) {

  def before(other: Time) : Boolean = {
    if (this.inMinutes < other.inMinutes) true else false
  }

  def inMinutes = hours * 60 + minutes
}

// Companion object from chapter 6
object Time {
  def apply(hours: Int, minutes: Int) = new Time(hours, minutes)
}

// 5.8
class Car(val manufacturer: String, val model: String, val year: Int = -1,
          var licensePlate: String = "")


object TrafficLightColor extends Enumeration {
  val Red, Yellow, Green = Value
}

// 8.1
class BankAccount2(initialBalance: Double) {
  private var balance = initialBalance
  def deposit(amount: Double) = { balance += amount; balance}
  def withdraw(amount: Double) = { balance -= amount; balance}
}

class CheckingAccount(checkFee: Double = 1, initialBalance: Double) extends
  BankAccount2(initialBalance: Double) {
  override def deposit(amount: Double) = super.deposit(amount + checkFee)
  override def withdraw(amount: Double) = super.withdraw(amount + checkFee)
}

// 8.2
class SavingsAccount(checkFee: Double = 1, interest: Double = 0.02,
                     initialBalance: Double = 0) extends
  BankAccount2(initialBalance: Double) {

  private var balance = initialBalance
  private var transactions = 0
  private var totalFees = 0

  def feesPaid = printf("Total fees: $%d", totalFees)

  def earnMonthlyInterest() = {
    balance *= (1 + interest)
    transactions += 1
    balance
  }

  def transaction(amt: Double, transType: Char = 'w') = {
    val amount = (transactions compare 3, transType) match {
      case (1 | 0, 'w') => { totalFees += 1; amt + 1 }
      case (1 | 0, 'd') => { totalFees += 1; amt - 1 }
      case _ => amt
    }
    transactions += 1
    if (transType == 'w') super.withdraw(amount) else super.deposit(amount)
  }

  override def deposit(amount: Double) = transaction(amount, 'd')
  override def withdraw(amount: Double) = transaction(amount, 'w')
}

val acct = new SavingsAccount(initialBalance = 10000)