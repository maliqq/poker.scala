package org.poker

import collection.mutable.HashMap
import collection.immutable.SortedMap
import collection.immutable.Map

import org.poker.hand._

class Hand(var cards: CardSet, ranking: Any) {
  
  def detect: Option[Rank] = ranking match {
      case Ranking.High => HighHand(cards)
      
      case Ranking.Badugi => BadugiHand(cards)
      
      case _: Ranking.Low.Value =>
      
        ranking match {
          case Ranking.Low.AceFive => LowHand.isAceFive(cards)

          case Ranking.Low.AceFive8 => LowHand.isAceFive8(cards)
          
          case Ranking.Low.AceSix => LowHand.isAceSix(cards)
          
          case Ranking.Low.DeuceSix => LowHand.isDeuceSix(cards)
          
          case Ranking.Low.DeuceSeven => LowHand.isDeuceSeven(cards)
        }
    }
  
}

object Hand {
  
  def apply[T](cards: CardSet, ranking: T): Option[Rank] = {
    (new Hand(cards, ranking)).detect
  }
  
}
