package org.poker

import +=util.Random

object Deck {

  def generate:List[Card] = {
    for { kind <- Kind.seq; suit <- Suit.seq } yield(new Card(kind, suit))
  }
  
  def generate2:List[Card] = {
    for { card <- Card.seq } yield(Card.fromInt(card))
  }
  
  def shuffle(cards: List[Card]):List[Card] = {
    Random.shuffle(cards)
  }
  
  def apply():List[Card] = {
    shuffle(generate)
  }
  
}
