package hitori

import com.hitori.{BitBoardRepresentation, Matrix, Point, Solver}
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

class BitBoardTest extends JUnitSuite {

  val test_1: Array[Array[Byte]] = Array(
    Array(1,1,1),
    Array(2,2,2),
    Array(3,3,3)
  ).map(_.map(_.toByte))

  @Test
  def checkColumnReprOne(): Unit = {
    val repr = new BitBoardRepresentation(3)
    repr.add(List(Point(0,2), Point(1,2), Point(2,2)))
    assertTrue(
      repr.getRowRepr.toVector == Vector(1,1,1) &&
      repr.getColumnRepr.toVector == Vector(0,0,7)
    )
  }

  @Test
  def checkColumnReprTwo(): Unit = {
    val repr = new BitBoardRepresentation(3)
    repr.add(List(Point(0,0), Point(1,1), Point(2,2)))
    assertTrue(
      repr.getRowRepr.toVector == Vector(4,2,1) &&
        repr.getColumnRepr.toVector == Vector(4,2,1)
    )
  }

  @Test
  def checkColumnReprThree(): Unit = {
    val repr = new BitBoardRepresentation(3)
    repr.add(List(Point(2,0), Point(2,1), Point(2,2)))
    assertTrue(
      repr.getRowRepr.toVector == Vector(0,0,7) &&
        repr.getColumnRepr.toVector == Vector(1,1,1)
    )
  }

  @Test
  def checkChargeCalculation(): Unit = {
    val repr = new BitBoardRepresentation(3)
    repr.add(List(Point(2,0), Point(2,1), Point(2,2)))
    assertFalse(repr.checkCharge)
    repr.clear()
    repr.add(List(Point(0,0), Point(1,1), Point(2,2)))
    assertTrue(repr.checkCharge)
    repr.add(List(Point(1,2)))
    assertFalse(repr.checkCharge)
  }

  @Test
  def checkWhitePointGeneration(): Unit = {
    val solver = new Solver(new Matrix(test_1))
    val map = solver.buildCoincidenceMap
    val pointSetMap = map.map {
      case (value, mapTuple) => (value, solver.toPointSet(mapTuple))
    }
    val pointSet = pointSetMap(1.toByte)
    assertTrue(solver.generateVariantsForWhitePoints(0, pointSet).size == 1)
    assertTrue(solver.generateVariantsForWhitePoints(1, pointSet).size == 3)
    assertTrue(solver.generateVariantsForWhitePoints(2, pointSet).isEmpty)
  }

}
