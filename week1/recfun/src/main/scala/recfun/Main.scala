package recfun

import java.util

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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def countBalance(chars: List[Char], stack: List[Char]): Boolean = {
      if (chars.isEmpty) return stack.isEmpty

      if (chars.head == '(') return countBalance(chars.tail, chars.head :: stack)
      if (chars.head == ')' && stack.isEmpty) return false
      if (chars.head == ')') return countBalance(chars.tail, stack.tail)

      countBalance(chars.tail, stack)
    }

    countBalance(chars, stack = List())
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) return 0
    if (money < 0) return 0
    if (money == 0) return 1

    countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
