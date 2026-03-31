package it.unibo.pps.polyglot.a01b

import it.unibo.pps.polyglot.OptionToOptional
import it.unibo.pps.polyglot.a01b.Logics
import it.unibo.pps.util.Optionals.Optional as ScalaOptional
import it.unibo.pps.util.Sequences.Sequence
import it.unibo.pps.util.Sequences.Sequence.Cons

import scala.jdk.javaapi.OptionConverters

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
trait ScalaLogics:
  def hitScala(x: Int, y: Int): ScalaOptional[Int]
  def won: Boolean


class LogicsImpl(private val size: Int, private val mines: Int) extends ScalaLogics with Logics:

  private var minesSet: Sequence[(Int, Int)] = Sequence.Nil()
  private var selected: Sequence[(Int, Int)] = Sequence.Nil()
  private val random = scala.util.Random(42)
  private var countHit: Int = 0 //per contare quante celle ho selezionato al momento

  //riempio minesSet (che mi dice le celle in cui si trovano le mine)
  var placedMines = 0
  while (placedMines < mines) {
    val newMine = (random.nextInt(size),random.nextInt(size))
    if !minesSet.contains(newMine) then {
      minesSet = Cons(newMine, minesSet)
      placedMines += 1
    }
  }

  private def findNeighbors(x: Int, y: Int): Int =
    var neighbors = 0
    for i <- -1 to 1 do
      for j <- -1 to 1 do
        if minesSet.contains((x+i, y+j)) then neighbors += 1

    neighbors


  def hitScala(x: Int, y: Int): ScalaOptional[Int] = (x,y) match {
    /* logica: se le coordinate si trovano nel minesSet, torno Empty
       altrimenti, aggiungo le coordinate a quelle selezionate e ritorno
       il numero di mine vicine (ma controllo non sia già tra le selected) */

    case _ if minesSet.contains((x,y)) => ScalaOptional.Empty()
    case _ =>
      if !selected.contains((x,y)) then {
        selected = Cons((x, y), selected)
        countHit += 1
      }
      ScalaOptional.Just(findNeighbors(x,y))
  }


  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    OptionToOptional(hitScala(x,y)) // Option => Optional converter

  def won = size*size == mines + countHit
