package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional

import util.SetADT.*
import util.Sequences.*
import Sequence.*
import util.Streams.*
import Stream.*

import polyglot.Pair

trait Logics:
  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won: Boolean

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private var minesSet: Set[Pair[Int, Int]] = fromSequence(Sequence.empty)
  private var selected: Set[Pair[Int, Int]] = fromSequence(Sequence.empty)
  private val random = scala.util.Random(42)

  for i <- 1 to mines do minesSet = minesSet.add(Pair(random.nextInt(size), random.nextInt(size)))

  // new methods for SetADT
  extension [A](s: Set[A])
    def add(a: A): Set[A] = if s.contains(a) then s else fromSequence(Cons(a, s.toSequence()))
    def size: Int = s.toSequence() match
      case Cons(_, t) => 1 + fromSequence(t).size
      case _ => 0

  private def neighbours(x: Int, y: Int): Int =
    fromSequence(
      iterate(x - 1)(_ + 1).take(3).flatMap(xx => iterate(y - 1)(_ + 1).take(3).map(yy => Pair(xx, yy)))
        .filter(p => minesSet.contains(p)).toList
    ).size

  override def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if minesSet.contains(Pair(x, y)) then
      OptionToOptional(ScalaOptional.Empty())
    else
      selected = selected.add(Pair(x, y))
      OptionToOptional(ScalaOptional.Just(neighbours(x, y)))

  override def won: Boolean = selected.size + minesSet.size == size * size
