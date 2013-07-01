package org.poker

import collection.immutable.SortedMap

object Detect {
  
  def apply(cards: CardSet, functions: SortedMap[_, (CardSet) => Option[Rank]], assignValue: Boolean = true) = {
    var rank: Option[Rank] = None
    functions.takeWhile(fn =>
      fn._2(cards) match {
        // FIXME remove options
        case Some(_rank) =>
          if (_rank.value == null && assignValue) {
            _rank.value = fn._1
          }
          rank = Some(_rank)
          false
        case None => true
      }
    )
    rank
  }
  
}
