import scala.annotation.tailrec
import scala.util.Failure

object Main {
  def main(args: Array[String]): Unit = {
    println(factorial1(10))
    println(factorial2(10))
    println(factorial3(10))
    println(factorial4(10))
    println(factorial5(10))
  }

  def factorial1 (n: Int) ={
    /*(n) match {
      case x if (x < 0) => Failure(new RuntimeException("Число должно быть положительное"))
      case x if (x > 500000) => Failure(new RuntimeException("Слишком большое число"))
      case _ => fun()
    }*/
    assert(n >= 0, "Число должно быть положительное")
    assert(n < 50000, "Слишком большое число")

    var f: BigInt = 1
    var a = 1
    def fun() = {
      while (a <= n){
        f *= a
        a += 1
      }
      f
    }
    fun()
  }

  def factorial2 (n: Int) = {//recursion
    assert(n >= 0, "Число должно быть положительное")
    assert(n < 50000, "Слишком большое число")

    @tailrec
    def fun(n: Int, acc: BigInt = 1): BigInt =
      if (n > 0) fun(n - 1, acc * n)
      else acc
    fun(n)
  }

  def factorial3 (n: Int) = {//foldLeft
    assert(n >= 0, "Число должно быть положительное")
    assert(n < 50000, "Слишком большое число")

    if (n == 0) 1
    else (1 to n).foldLeft(BigInt(1))((acc, e) => acc * e)//or (1 to n).foldLeft(BigInt(1))(_*_)

  }

  def factorial4 (n: Int) = {//foldLeft
    assert(n >= 0, "Число должно быть положительное")
    assert(n < 50000, "Слишком большое число")

    if (n == 0) 1
    else (BigInt(1) to BigInt(n)).reduce(_ * _)//or (1 to n).foldLeft(BigInt(1))(_*_)
  }

  def factorial5 (n: Int) = {//foldLeft
    assert(n >= 0, "Число должно быть положительное")
    assert(n < 50000, "Слишком большое число")

    if (n == 0) 1
    else (BigInt(1) to BigInt(n)).product
  }

}