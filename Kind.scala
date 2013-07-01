package org.poker

object Kind extends Enumeration {

  type Kind = Value
  val Deuce, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
  
  implicit def kind2Int(kind: Kind): Int = {
    Kind.values.toList.indexOf(kind)
  }
  
  implicit def int2Kind(kind: Int): Kind = {
    Kind.values.toList(kind)
  }
  
  val ace: Int = Ace
  val seq: List[Int] = List.range(0, ace + 1)
  val kinds: List[Char] = "23456789TJQKA".toList
  val names: List[String] = List("deuce", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "jack", "queen", "king", "ace")

  def format(fmt: String, kind: Int): String = {
    fmt match {
      case "%A" =>
        format("%A", kind).capitalize
      
      case "%a" =>
        names(kind)
    }
  }

}
