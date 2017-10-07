package hitori

import com.hitori.{BitBoardRepresentation, Matrix, Point, Solver}
import org.junit.Test
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

  val test_2: Array[Array[Byte]] = Array(
    Array(1,0,0),
    Array(1,0,0),
    Array(2,0,0)
  ).map(_.map(_.toByte))

  val test_3: Array[Array[Byte]] = Array(
    Array(1,1,0),
    Array(2,0,0),
    Array(3,0,0)
  ).map(_.map(_.toByte))

  val test_4: Array[Array[Byte]] = Array(
    Array(1,0,1),
    Array(0,1,0),
    Array(1,0,1)
  ).map(_.map(_.toByte))

  @Test
  def checkColumnReprOne(): Unit = {
    val repr = new BitBoardRepresentation(3, List(Point(0,2), Point(1,2), Point(2,2)))
    assertTrue(
      repr.getRowRepr.toVector == Vector(1,1,1) &&
      repr.getColumnRepr.toVector == Vector(0,0,7)
    )
  }

  @Test
  def checkColumnReprTwo(): Unit = {
    val repr = new BitBoardRepresentation(3, List(Point(0,0), Point(1,1), Point(2,2)))
    assertTrue(
      repr.getRowRepr.toVector == Vector(4,2,1) &&
        repr.getColumnRepr.toVector == Vector(4,2,1)
    )
  }

  @Test
  def checkColumnReprThree(): Unit = {
    val repr = new BitBoardRepresentation(3, List(Point(2,0), Point(2,1), Point(2,2)))
    assertTrue(
      repr.getRowRepr.toVector == Vector(0,0,7) &&
        repr.getColumnRepr.toVector == Vector(1,1,1)
    )
  }

  @Test
  def checkChargeCalculation(): Unit = {
    val repr1 = new BitBoardRepresentation(3, List(Point(2,0), Point(2,1), Point(2,2)))
    assertFalse(repr1.checkCharge)
    val repr2 = new BitBoardRepresentation(3, List(Point(0,0), Point(1,1), Point(2,2)))
    assertTrue(repr2.checkCharge)
    val repr3 = new BitBoardRepresentation(3, List(Point(0,0), Point(1,1), Point(2,2), Point(1,2)))
    assertFalse(repr3.checkCharge)
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

  @Test
  def checkGenerationForOneValue(): Unit = {
    val solver = new Solver(new Matrix(test_1))
    val map = solver.buildCoincidenceMap
    val pointSetMap = map.map {
      case (value, mapTuple) => (value, solver.toPointSet(mapTuple))
    }
    val pointSet = pointSetMap(1.toByte)
    assertTrue(solver.generateAllVariantsForCertainValue(pointSet).size == 4)
  }

  @Test
  def checkCheckNeighborhood(): Unit = {
    val solver2 = new Solver(new Matrix(test_2))
    val pointList2 = solver2.buildCoincidenceMap.map {
      case (value, mapTuple) => (value, solver2.toPointSet(mapTuple))
    }.get(1.toByte).get.toList

    val bitBoard2 = new BitBoardRepresentation(3, pointList2)
    assertFalse(bitBoard2.checkNeighborhood)

    val solver3 = new Solver(new Matrix(test_3))
    val pointList3 = solver3.buildCoincidenceMap.map {
      case (value, mapTuple) => (value, solver3.toPointSet(mapTuple))
    }.get(1.toByte).get.toList

    val bitBoard3 = new BitBoardRepresentation(3, pointList3)
    assertFalse(bitBoard3.checkNeighborhood)

    val solver4 = new Solver(new Matrix(test_4))
    val pointList4 = solver4.buildCoincidenceMap.map {
      case (value, mapTuple) => (value, solver4.toPointSet(mapTuple))
    }.get(1.toByte).get.toList

    val bitBoard4 = new BitBoardRepresentation(3, pointList4)
    assertTrue(bitBoard4.checkNeighborhood)

  }

}
