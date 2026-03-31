package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.a05b.Logics

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private val random = scala.util.Random(42)

  private var tickCount : Int = 0

  private val (initialX, initialY) : (Int, Int) = (random.nextInt(size-2)+1,random.nextInt(size-2)+1)

  override def tick(): Unit = tickCount += 1

  override def isOver: Boolean =
    initialY - tickCount < 0 || initialY + tickCount >= size ||
      initialX - tickCount < 0 || initialX + tickCount >= size

  override def hasElement(x: Int, y: Int): Boolean =
    (x == initialX && math.abs(y - initialY) <= tickCount) ||
      (y == initialY && math.abs(x - initialX) <= tickCount) ||
      (x-y == initialX - initialY && math.abs(x - initialX) <= tickCount) ||
      (x+y == initialX + initialY && math.abs(x - initialX) <= tickCount)
