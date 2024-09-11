package recfun
import common._

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    print(countChange(4,List(1,2)))
    println()
    print(countChange(300,List(5,10,20,50,100,200,500)))
    println()
    print(countChange(301,List(5,10,20,50,100,200,500)))
    println()
    print(countChange(300,List(500,5,50,100,20,200,10)))
  }

  def pascal(c: Int, r: Int): Int = {
    if(r<0 || c<0 || r<c) throw new RuntimeException("pascal: Input is Invalid")
    else if(c==0 || r==0 || c==r) 1
    else pascal(c-1,r-1)+pascal(c,r-1)
  }


  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def _balance(chars: List[Char], pars: Int): Boolean = {
      if(pars < 0) false
      else if(chars.isEmpty && pars == 0) true
      else if(chars.isEmpty && pars > 0) false
      else if(chars.head == '(') _balance(chars.tail, pars+1)
      else if(chars.head == ')') _balance(chars.tail, pars-1)
      else false
    }
    _balance(chars, 0)
  }


  def countChange(money: Int, coins: List[Int]): Int = {
    def _countChange(money: Int, coins: List[Int]): Int ={
      if(money == 0) 1
      else if(coins.isEmpty || money < 0) 0
      else _countChange(money-coins.head, coins) + _countChange(money, coins.tail)
    }
    val sortCoin = coins.sorted
    if(sortCoin.head <= 0 || money < 0) throw new RuntimeException("countChange: Input is Invalid")
    else _countChange(money, sortCoin)
  }
}
