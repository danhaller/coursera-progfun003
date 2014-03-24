package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def count(current : Int, remaining: List[Char]) : Boolean = {
      if (current < 0) false
      else if (remaining.isEmpty) true
      else remaining.head match {
        case '(' => count(current+1, remaining.tail)
        case ')' => count(current-1, remaining.tail)
        case _ => count(current, remaining.tail)
      }
    }

    count(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money-coins.head, coins)
  }
}
