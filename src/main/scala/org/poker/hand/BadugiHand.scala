package org.poker.hand

import org.poker._
import collection.immutable.SortedMap

object BadugiHand extends RankedHand {

  var steps = SortedMap(
      Ranking.Badugi.One -> ((cards) => isOne(cards)),
      Ranking.Badugi.Four -> ((cards) => isFour(cards)),
      Ranking.Badugi.Three -> ((cards) => isThree(cards)),
      Ranking.Badugi.Two -> ((cards) => isTwo(cards))
    )
    
  def apply(cards: CardSet): Option[Rank] = {
    Detect(cards, steps)
  }
  
  def isOne(cards: CardSet): Option[Rank] = {
    if (cards.groupedByKind.size == 1) {
      
      Rank(cards, List(cards.head))
    
    } else if (cards.groupedBySuit.size == 1) {
    
      Rank(cards, List(cards.groupedBySuit.head._2.min(cards.ordering)))

    } else {
      None
    }
  }

  def isTwo(cards: CardSet): Option[Rank] = {
    var a: Card = null; var b: Card = null
    
    if (cards.paired.contains(3)) {
      val _cards = cards.paired(3).head.asInstanceOf[List[Card]]
    
      b = (cards.toSet -- _cards.toSet).toList.head
      a = _cards.filter(_.suit != b.suit).head
    
    } else if (cards.suited.contains(3)) {
      val _cards = cards.suited(3).head.asInstanceOf[List[Card]]
      
      a = (cards.toSet -- _cards.toSet).toList.head
      b = _cards.filter(_.kind != a.kind).min(cards.ordering)
    
    } else if (!cards.groupedBySuit.isEmpty) {
      val _cards = cards.groupedBySuit.head._2
    
      a = _cards.min(cards.ordering)
      b = (cards.toSet -- _cards.toSet).toList.
        filter(c => (c.suit != a.suit) && (c.kind != a.kind)).min(cards.ordering)
    
    } else {
      val _cards = cards.groupedByKind.head._2
    
      a = _cards.head
      b = (cards.toSet -- _cards.toSet).toList.filter(_.kind != a.kind).min(cards.ordering)
    }
    
    Rank(cards, List(a, b).sorted(cards.ordering))
  }
  
  def isThree(cards: CardSet): Option[Rank] = {
    val _paired = if (cards.paired.contains(2)) cards.paired(2) else List.empty
    val _suited = if (cards.suited.contains(2)) cards.suited(2) else List.empty
    
    if (_suited.isEmpty && _paired.isEmpty) {
      return None
    } 
    
    val (a: Card, b: Card, c: Card) = if (_paired.length == 1 && _suited.length != 2) {
      var _cards = _paired.head
      var a = _cards.head
      
      val List(b, c, _*) = (cards.cards.toSet -- _cards.toSet).toList.filter(_.kind != a.kind)
    
      if (b.suit == c.suit) {
        return None
      }
      if (b.suit == a.suit || c.suit == a.suit) {
        a = _cards(1)
      }
      (a, b, c)
    
    } else if (_paired.isEmpty && _suited.length == 1) {
      var a = _suited.head.min(cards.ordering)
  
      val List(b, c, _*) = (cards.toSet -- _suited.head.toSet).toList.filter(_.suit != a.suit)
      
      if (b.kind == c.kind) {
        return None
      }
      (a, b, c)
    
    } else {
      return None
    }
    Rank(cards, List(a, b, c).sorted(cards.ordering))
  }

  def isFour(cards: CardSet): Option[Rank] =
    if (cards.groupedByKind.size == 4 && cards.groupedBySuit.size == 4) {
      
      Rank(cards, cards.sorted(cards.ordering)) // FIXME fix for cards.length > 4
    
    } else None

}