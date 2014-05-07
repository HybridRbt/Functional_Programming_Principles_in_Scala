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
  def pascal(c: Int, r: Int): Int =
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def num_p(acc: Int, chars: List[Char]): Int = {
      if (chars.isEmpty) acc
      else if (acc < 0) acc
      else if (chars.head == '(') num_p(acc+1, chars.tail)
      else if (chars.head == ')') num_p(acc-1, chars.tail)
      else num_p(acc, chars.tail)
    }

    if (num_p(0, chars) == 0) true
    else false
  }

  /**
   * Exercise 3
   */

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      countChange(money - coins.sortWith(_>_).head, coins) + countChange(money, coins.sortWith(_>_).tail)
    }
  }
}
