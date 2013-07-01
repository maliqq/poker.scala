package org.poker

import org.poker._
import collection.immutable.Map

class Rank {
  
  import CardSet._
  
  var value: Any = null
  
  var cards: CardSet = List.empty // original cards
  var valueCards: CardSet = List.empty
  var highCards: CardSet = List.empty
  var kickerCards: CardSet = List.empty
  
  override def toString = {
    "%s(high=%s value=%s kickers=%s)".format(value.toString, highCards, valueCards, kickerCards)
  }

}

case class RankOptions(
  high: Option[List[Card]] = None,
  kickers: Boolean = false
  )

object Rank {
  def apply(cards: CardSet, value: CardSet, options: RankOptions = new RankOptions): Option[Rank] = {
    val rank = new Rank
    
    rank.cards = cards
    rank.valueCards = value
    
    rank.highCards = if (options.high.isDefined) {
      options.high.get
    } else List(value.head)
    
    if (options.kickers) {
      rank.kickerCards = cards.diff(rank.valueCards).sorted(cards.ordering.reverse).take(5 - rank.valueCards.length)
    }

    Some(rank)
  }
}


object RankOrdering extends Ordering[Rank] {
  
  def compare(a: Rank, b: Rank): Int = {
    val comparers = List(
      (() => compareRank(a, b)),
      (() => compareHighCards(a, b)),
      (() => compareValueCards(a, b)),
      (() => compareKickerCards(a, b))
    )
    var result = 0
    comparers takeWhile { comparer =>
      result = comparer.apply
      result == 0
    }
    result
  }
  
  // FIXME
  def compareRank(a: Rank, b: Rank) = 1
  
  def compareHighCards(a: Rank, b: Rank): Int = {
    compareCards(a.highCards, b.highCards)
  }
  
  def compareValueCards(a: Rank, b: Rank): Int = {
    compareCards(a.valueCards, b.valueCards)
  }
  
  def compareKickerCards(a: Rank, b: Rank): Int = {
    compareCards(a.kickerCards, b.kickerCards)
  }
  
  def compareCards(a: CardSet, b: CardSet): Int = {
    if (a.length == b.length) {
      var result = 0
      a.zipWithIndex.takeWhile { case (card, i) =>
        result = a.ordering.indexOf(card) compare b.ordering.indexOf(b(i)) // FIXME low cards
        result == 0
        }
      result
    } else {
      val l = Math.min(a.length, b.length)
      compareCards(a.take(l), b.take(l))
    }
  }
  
}
