package polyglot.a05b

import polyglot.a05b.Logics
import polyglot.Pair

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  private val random = scala.util.Random(42)
  private val initial: Pair[Int, Int] = Pair(random.nextInt(size - 2) + 1, random.nextInt(size - 2) + 1)
  private var tickCount: Int = 0

  override def tick(): Unit = tickCount = tickCount + 1

  override def isOver: Boolean =
    initial.getY() - tickCount < 0 || initial.getY() + tickCount >= size
      || initial.getX() - tickCount < 0 || initial.getX()+tickCount >= size

  override def hasElement(x: Int, y: Int): Boolean =
    (x == initial.getX() && Math.abs(y - initial.getY()) <= tickCount) ||
      (y == initial.getY() && Math.abs(x - initial.getX()) <= tickCount) ||
      (x - y == initial.getX() - initial.getY() && Math.abs(x - initial.getX()) <= tickCount) ||
      (x + y == initial.getX() + initial.getY() && Math.abs(x - initial.getX()) <= tickCount)
