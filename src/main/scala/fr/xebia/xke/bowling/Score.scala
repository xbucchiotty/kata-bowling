package fr.xebia.xke.bowling

import scala.collection.immutable.::

//
//TRY
//
sealed trait Shot

sealed trait FirstShot {
  def down: Int
}

case object Gutter extends Shot with FirstShot {
  val down = 0

  val symbol = '-'
}

case object TenDown extends Shot with FirstShot {
  val down = 10
}

case object SpareShot extends Shot

class Hit private(val down: Int) extends Shot with FirstShot {
  override def toString = s"Hit($down)"
}

object Hit {
  def unapply(str: Char): Option[Hit] =
    if (str.isDigit && str.toString.toInt > 0) {
      Some(new Hit(str.toString.toInt))
    } else {
      None
    }

}


//
//TURN
//
sealed trait Turn {
  def first: FirstShot

  def down: Int
}

case object Strike extends Turn with ((Option[Turn], Option[Turn]) => Score) {
  val first = TenDown

  val down = first.down

  val symbol = 'X'

  override def apply(v1: Option[Turn], v2: Option[Turn]): Score = {
    (for {
      nextTurn <- v1
      nextNextTurn <- v2
    } yield down + nextTurn.down + nextNextTurn.down).getOrElse(0)
  }

  def unapply(strs: (Char, Char)): Option[Strike.type] = strs match {
    case (`symbol`, _) => Some(Strike)
    case _ => None
  }

  override def toString() = "Strike"

}

sealed trait DoubleTry extends Turn {
  def second: Shot

}

sealed trait Spare extends DoubleTry with (Option[Turn] => Score) {

  override def apply(v1: Option[Turn]): Score = v1.map(_.first.down + this.down).getOrElse(0)

  val down = TenDown.down

  override def toString = "Spare"

}

object Spare {
  val symbol = '/'
}

case object GutterThenSpare extends Spare {
  val first = Gutter
  val second = TenDown

}

case class RegularSpare(first: Hit) extends Spare {
  val second = SpareShot
}

sealed trait NotSpare extends DoubleTry with (() => Score) {
  override def apply(): Score = down

  override def toString = "NotSpare"
}

case object DoubleGutter extends NotSpare {
  val first = Gutter
  val second = Gutter

  val down = Gutter.down
}

case class GutterThenHit(second: Hit) extends NotSpare {
  val first = Gutter

  val down = second.down
}

case class HitThenGutter(first: Hit) extends NotSpare {
  val second = Gutter

  val down = first.down
}

class RegularDoubleTry private(val first: Hit, val second: Hit) extends NotSpare {
  def down = first.down + second.down
}

object RegularDoubleTry {
  def unapply(first: Hit, second: Hit): Option[RegularDoubleTry] = {
    if (first.down + second.down < TenDown.down) {
      Some(new RegularDoubleTry(first, second))
    } else {
      None
    }
  }
}

case class BonusShot(first: FirstShot) extends Turn {

  override def down: Int = first.down
}

object Parser {

  def parse(str: String): Option[List[Shot]] = Ops.sequence(str.toCharArray.toList.map {
    case Strike.symbol => Some(TenDown)
    case Gutter.symbol => Some(Gutter)
    case Spare.symbol => Some(SpareShot)
    case Hit(h) => Some(h)
    case _ => None
  })

  def shotsToTurn(shots: List[Shot]): List[Option[Turn]] = shots match {
    case (bonus: FirstShot) :: Nil => Some(BonusShot(bonus)) :: Nil
    case TenDown :: tail => Some(Strike) :: shotsToTurn(tail)
    case Gutter :: SpareShot :: tail => Some(GutterThenSpare) :: shotsToTurn(tail)
    case (h: Hit) :: SpareShot :: tail => Some(RegularSpare(h)) :: shotsToTurn(tail)
    case Gutter :: Gutter :: tail => Some(DoubleGutter) :: shotsToTurn(tail)
    case Gutter :: (h: Hit) :: tail => Some(GutterThenHit(h)) :: shotsToTurn(tail)
    case (h: Hit) :: Gutter :: tail => Some(HitThenGutter(h)) :: shotsToTurn(tail)
    case (first: Hit) :: (second: Hit) :: tail => RegularDoubleTry.unapply(first, second) :: shotsToTurn(tail)
    case Nil => Nil
    case _ => List(None)
  }

  def toTurns(shots: List[Shot]): Option[List[Turn]] = Ops.sequence(shotsToTurn(shots))

  def run(str: String): Option[Int] = for {
    shots <- parse(str)
    turns <- toTurns(shots)
  } yield {

    val numberOfTurns = turns.drop(9) match {
      case Strike :: Strike :: _ => 12
      case Strike :: _ => 11
      case (s: Spare) :: _ => 11
      case _ => 10
    }

    Ops.traverseBy3(turns.take(numberOfTurns)).foldLeft(0) {
      case (score, turnsForScore) => score + (turnsForScore match {
        case (Strike, second, third) =>
          Strike.apply(second, third)

        case ((spare: Spare), second, _) =>
          spare.apply(second)

        case ((regular: NotSpare), _, _) =>
          regular.down

        case ((regular: BonusShot), _, _) =>
          0
      })
    }
  }
}


object Ops {

  def sequence[A](list: List[Option[A]]): Option[List[A]] = list.foldLeft(Option(List.empty[A])) {
    case (acc, elt) => for {
      nonEmptyElements <- acc
      nonEmptyElement <- elt
    } yield nonEmptyElement :: nonEmptyElements
  }.map(_.reverse)

  def traverseBy3[A](list: List[A]): List[(A, Option[A], Option[A])] = {
    list.take(3) match {
      case elt1 :: elt2 :: elt3 :: tail => (elt1, Some(elt2), Some(elt3)) :: traverseBy3(list.tail)
      case elt1 :: elt2 :: Nil => (elt1, Some(elt2), None) :: traverseBy3(list.tail)
      case elt1 :: Nil => (elt1, None, None) :: traverseBy3(list.tail)
      case Nil => Nil
    }
  }
}