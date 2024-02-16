package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()


  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || c == r then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def isBalance(chars: List[Char], pCount: Int): Boolean =
      if pCount < 0 then false
      else if chars.isEmpty then pCount == 0
      else if chars.head == '(' then isBalance(chars.tail, pCount + 1)
      else if chars.head == ')' then isBalance(chars.tail, pCount - 1)
      else isBalance(chars.tail, pCount)

    isBalance(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
//    println(s"money: $money, coins: $coins")
    if money == 0 then 1
    else if coins.isEmpty || money < 0 then 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)