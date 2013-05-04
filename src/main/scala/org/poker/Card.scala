package poker

object Card {
  
  val seq = List.range(0, 52)
  
  implicit def fromInt(mask: Int): Card = {
    new Card(mask >> 2, mask & 3)
  }

  implicit def fromString(str: String): Card = {
    if (str.length == 2) {
      val List(kind, suit) = str.toList
      new Card(Kind.kinds.indexOf(kind), Suit.suits.indexOf(suit))
    } else {
      throw(new Error("Can't parse card from string: %s".format(str)))
    }
  }
  
  def apply(any: Any): Card = {
    any match {
      case i: Int => fromInt(i)
      case s: String => fromString(s)
    }
  }

}

class Card(val kind: Int, val suit: Int) {
  
  def toInt = (kind << 2) + suit
  
  //override def hashCode = toInt
  
  def equals(other: Card): Boolean = {
    other match {
      case card: Card => other.kind == kind && other.suit == suit
      case _ => false
    }
  }
  
  override def toString = "%s%s".format(Kind.kinds(kind), Suit.format("%b", suit))
  //override def toString = colored
  
  def colored = {
    "%s%s%s%s ".format(Suit.colors(suit), Kind.kinds(kind), Suit.format("%b", suit), Console.RESET)
  }
  
}

trait CardOrdering extends Ordering[Card] {
  
  def compare(a: Card, b: Card) = indexOf(a) compare indexOf(b)
  
  def indexOf(card: Card): Int = card.kind + 1
  
  def ordering = this
  
  def byMax: Ordering[List[Card]] = new Ordering[List[Card]] {
    def compare(a: List[Card], b: List[Card]) = {
      indexOf(a.max(ordering)) compare indexOf(b.max(ordering))
    }
  }
  
  def byHead: Ordering[List[Card]] = new Ordering[List[Card]]{
    def compare(a: List[Card], b: List[Card]) = {
      indexOf(a.head) compare indexOf(b.head)
    }
  }

}

object AceHighOrdering extends CardOrdering {}

object AceLowOrdering extends CardOrdering {
  
  override def indexOf(card: Card): Int = {
    if (card.kind == Kind.kinds.indexOf('A')) 0 else card.kind + 1
  }
  
}
