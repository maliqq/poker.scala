package org.poker

object Ranking {
  
  object High extends Enumeration {
    type High = Value
    val StraightFlush, FourKind, FullHouse, Flush, Straight, ThreeKind, TwoPair, OnePair, HighCard = Value
  }
  import High._
  
  object Low extends Enumeration {
    type Low = Value
    val CompleteLow, IncompleteLow, High = Value
    val AceFive, AceFive8, AceSix, DeuceSix, DeuceSeven = Value
  }
  import Low._
  
  object Badugi extends Enumeration {
    type Badugi = Value
    val One, Four, Three, Two = Value
  }
  import Badugi._
  
}
