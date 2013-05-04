package org.poker

import scala.util.matching.Regex

object CardSet {
  
  implicit def cardSet2List(set: CardSet): List[Card] = set.cards 
  implicit def list2CardSet(cards: List[Card]): CardSet = CardSet(cards)

  case class BinaryCards(list: List[Int])

  case class ListCards(list: List[Card])
  
  def apply(s: Object): CardSet = {
    val cards: List[Card] = s match {
      case s: String =>
      
        val regex = """(?i)([akqjt2-9]{1})([schd]{1})""".r
        (for {
          regex(kind, suit) <- regex findAllIn s.toString
        } yield new Card(Kind.kinds.indexOf(kind(0)), Suit.suits.indexOf(suit(0)))).toList

      case BinaryCards(s) =>
        (for { byte <- s } yield Card.fromInt(byte)).toList

      case ListCards(s) => s
      
      case _ =>
        throw(new Error("Can't parse cards"))
    }
    new CardSet(cards)
  }
  
  def empty = CardSet(List.empty)

  def groupByLength(group: Map[Int, List[Card]]): Map[Int, List[List[Card]]] = {
    group.toList.

    map { g1 =>
      (g1._2.length, g1._2)
    }.groupBy(_._1).
    
    map { g2 =>
      (g2._1, g2._2.map(_._2).toList)
    }
  }
}

case class CardSet(val cards: List[Card]) {
  
  var ordering: CardOrdering = AceHighOrdering
  
  def qualifiedBy(qualifier: Char): CardSet = {
    val newCards = cards.filter(ordering.indexOf(_) <= Kind.kinds.indexOf(qualifier))
    copy(cards = newCards)
  }
  
  def groupedByGap: List[List[Card]] = {
    val _cards =  cards.filter(_.kind == Kind.kinds.indexOf('A')) ++ cards
    
    var _gaps: List[List[Card]] = List[List[Card]]()
    
    val buffer:List[Card] = _cards.foldLeft(List[Card]()) { case (_buffer: List[Card], item: Card) =>
      
      val prev:Option[Card] = if (_buffer.isEmpty) None else Some(_buffer.last)
      
      if (!prev.isDefined ||
          (AceHighOrdering.indexOf(prev.get) + 1 == AceHighOrdering.indexOf(item)) ||
          (AceLowOrdering.indexOf(prev.get) + 1 == AceLowOrdering.indexOf(item))) { // FIXME lazy?
        _buffer ++ List[Card](item)
      } else if (prev == item) {
        _buffer
      } else {
        _gaps ++= List[List[Card]](_buffer)
        List.empty
      }
    }
    _gaps ++ List[List[Card]](buffer)
  }

  lazy val groupedBySuit = cards.groupBy(_.suit)
  lazy val groupedByKind = cards.groupBy(_.kind)

  lazy val suited = CardSet.groupByLength(groupedBySuit)
  lazy val paired = CardSet.groupByLength(groupedByKind)

  lazy val gaps = groupedByGap
  
  override def toString = cards.map(_.toString).mkString("")
  
}
