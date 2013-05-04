package org.poker

object Suit extends Enumeration {

  type Suit = Value
  val Spade, Heart, Diamond, Club = Value
  
  implicit def suit2Int(suit: Suit): Int = {
    Suit.values.toList.indexOf(suit)
  }
  
  implicit def int2Suit(suit: Int): Suit = {
    Suit.values.toList(suit)
  }
  
  val seq = List.range(0, 4)
  val suits: List[Char] = "shdc".toList
  val names = List("spade", "heart", "diamond", "club")
  val unicodeChars = (List(0x2660, 0x2665, 0x2666, 0x2663).map { c => c.toChar.toString }).toList
  val colors = (List(Console.YELLOW, Console.RED, Console.CYAN, Console.GREEN))
  
  def format(fmt: String, suit: Int): String = {
    fmt match {
      case "%A" =>
        format("%a", suit).capitalize

      case "%b" =>
        unicodeChars(suit)
      
      case "%a" =>
        names(suit)
    }
  }

}
