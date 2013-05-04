package org.poker.hand

import org.poker._
import collection.immutable.SortedMap

object HighHand extends RankedHand {
  
  def apply(cards: CardSet): Option[Rank] = {
    Detect(cards, SortedMap(
      Ranking.High.StraightFlush -> ((cards) => isStraightFlush(cards)),
      Ranking.High.ThreeKind -> ((cards) => isThreeKind(cards)),
      Ranking.High.TwoPair -> ((cards) => isTwoPair(cards)),
      Ranking.High.OnePair -> ((cards) => isOnePair(cards)),
      Ranking.High.HighCard -> ((cards) => isHighCard(cards))
    ))
  }
  
  def isStraightFlush(cards: CardSet): Option[Rank] = {
    val possibleFlush: Option[Rank] = isFlush(cards)
    if (!possibleFlush.isDefined) {
      return Detect(cards, SortedMap(
        Ranking.High.FourKind -> ((cards) => isFourKind(cards)),
        Ranking.High.FullHouse -> ((cards) => isFullHouse(cards)),
        Ranking.High.Straight -> ((cards) => isStraight(cards))
      ))
    }
    
    val possibleStraight = isStraight(possibleFlush.get.valueCards) // FIXME
    if (possibleStraight.isDefined) {
      return possibleStraight // straight or royal flush
    }
    
    val possibleHigher = Detect(cards, SortedMap(
      Ranking.High.FourKind -> ((cards) => isFourKind(cards)),
      Ranking.High.FullHouse -> ((cards) => isFullHouse(cards))
    ))
      
    if (possibleHigher.isDefined) {
      return possibleHigher
    }
      
    possibleFlush.get.value = Ranking.High.Flush
    possibleFlush
  }
  
  def isFourKind(cards: CardSet): Option[Rank] = {
    if (!cards.paired.contains(4)) {
      return None
    }
    
    Rank(cards, cards.paired(4).head, RankOptions(kickers = true))
  }

  def isFullHouse(cards: CardSet): Option[Rank] = {
    
    if (!cards.paired.contains(3)) {
      return None
    }
    
    val sets: List[List[Card]] = cards.paired(3)
    
    var minor: List[Card] = null
    var major: List[Card] = null
    
    if (sets.length > 1) {
      val List(_major, _minor, _*) = sets.sorted(cards.ordering.byHead.reverse)
      major = _major
      minor = _minor.take(2)
    } else {
      if (!cards.paired.contains(2)) {
        return None
      }
      
      val pairs: List[List[Card]] = cards.paired(2)
      
      major = sets.head
      minor = pairs.sorted(cards.ordering.byHead.reverse).head
    }
    
    Rank(cards, major ++ minor, RankOptions(
        high = Some(List(major.head, minor.head)),
        kickers = true
      ))
  }
  
  def isFlush(cards: CardSet): Option[Rank] = {
    val groups = cards.groupedBySuit.filter(g => g._2.length >= 5)
    
    if (groups.isEmpty) {
      return None
    }
    
    Rank(cards, groups.head._2.sorted(cards.ordering).reverse.take(5))
  }
  
  def isStraight(cards: CardSet): Option[Rank] = {
    val gaps = cards.gaps.filter(_.length >= 5)
    
    if (gaps.isEmpty) {
      return None
    }
    
    val row = gaps.head
    if (row.last.kind == Kind.kinds.indexOf('5')) {
      // wheel straight
      return Rank(cards, row.reverse, RankOptions(
          high = Some(List(row.last))
        ))
    }
    
    Rank(cards, row.sorted(cards.ordering.reverse).take(5), RankOptions(
        high = Some(List(row.max(cards.ordering)))
      ))
  }
  
  def isThreeKind(cards: CardSet): Option[Rank] = {
    if (!cards.paired.contains(3)) {
      return None
    }
    
    val _cards = cards.paired(3)
    if (_cards.length != 1) {
      return None
    }
    
    Rank(cards, _cards.head, RankOptions(kickers = true))
  }
  
  def isTwoPair(cards: CardSet): Option[Rank] = {
    if (!cards.paired.contains(2)) {
      return None
    }
    
    val _cards = cards.paired(2)
    
    if (_cards.length < 2) {
      return None
    }
    
    val List(minor, major, _*) = _cards.sorted(cards.ordering.byMax)
    
    Rank(cards, major ++ minor, RankOptions(
        high = Some(List(major.max(cards.ordering), minor.max(cards.ordering))),
        kickers = true
      ))
  }
  
  def isOnePair(cards: CardSet): Option[Rank] = {
    if (!cards.paired.contains(2)) {
      return None
    }
    
    val _cards = cards.paired(2)
    
    if (_cards.length != 1) {
      return None
    }
    
    Rank(cards, _cards.head, RankOptions(kickers = true))
  }
  
  def isHighCard(cards: CardSet): Option[Rank] = {
    Rank(cards, List(cards.max(cards.ordering)), RankOptions(kickers = true))
  }

}
