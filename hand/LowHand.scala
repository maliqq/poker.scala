package org.poker.hand

import org.poker._

object LowHand extends RankedHand {
  
  def apply(cards: CardSet): Option[Rank] = isLow(cards)

  def isLow(cards: CardSet): Option[Rank] = {
    
    val _cards = cards.groupedByKind.map(_._2).map(_.head).toList.sorted(cards.ordering).slice(0, 5)
    
    if (_cards.isEmpty) {
      return None
    }
    
    val rank = Rank(cards, _cards, RankOptions(
        high = Some(List(_cards.max(cards.ordering)))
      ))
    
    if (_cards.length == 5) {
      rank.get.value = Ranking.Low.CompleteLow
    } else {
      rank.get.value = Ranking.Low.IncompleteLow
    }
    rank
    
  }

  def isGapLow(cards: CardSet): Option[Rank] = {
    val high: Option[Rank] = HighHand(cards)
    
    if (high.get.value == Ranking.High.HighCard) {
      isLow(cards)
    } else high
  }

  def isAceFive(cards: CardSet): Option[Rank] = {
    cards.ordering = AceLowOrdering
    isLow(cards)
  }

  def isAceFive8(cards: CardSet): Option[Rank] = {
    cards.ordering = AceLowOrdering
    isLow(cards.qualifiedBy('8'))
  }
  
  def isDeuceSix(cards: CardSet): Option[Rank] = {
    isLow(cards)
  }

  def isDeuceSeven(cards: CardSet): Option[Rank] = {
    isGapLow(cards)
  }

  def isAceSix(cards: CardSet): Option[Rank] = {
    cards.ordering = AceLowOrdering
    isGapLow(cards)
  }

}
