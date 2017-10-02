package com.solver

import java.util

import com.hitori.Matrix


/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

sealed trait SuperPoint
case object Pole extends SuperPoint
case class Point(row: Byte, column: Byte) extends Ordered[Point] with SuperPoint {

  import scala.math.Ordered.orderingToOrdered

  def compare(that: Point): Int = (this.row, this.column) compare(that.row, that.column)
}

final class UndirectedEdge(val point1: SuperPoint, val point2: SuperPoint) {
  override def hashCode(): Int = point1.hashCode() | point2.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: UndirectedEdge => (that.point1==this.point1 && this.point2 == that.point2) ||
      (that.point1==this.point2 && this.point2 == that.point1)
    case _ => false
  }

  override def toString: String = s"UndirectedEdge(${this.point1}, ${this.point2})"

  def getOtherVertex(point: SuperPoint): SuperPoint = {
    if(point == point1) point2
    else if(point == point2) point1
    else throw new IllegalArgumentException("Edge not contain this point")
  }
}

sealed trait Node {
  val point: Point
}

sealed trait RowNodeTrait extends Node

sealed trait ColumnNodeTrait extends Node

case class RowNode(override val point: Point, columnList: List[ColumnNode]) extends RowNodeTrait

case class ColumnNode(override val point: Point, rowList: List[RowNode]) extends ColumnNodeTrait

case class HeadNode(override val point: Point, columnList: List[ColumnNode], rowList: List[RowNode]) extends RowNodeTrait with ColumnNodeTrait

sealed trait Color

case object White extends Color

case object Black extends Color

class BitBoardRepresentation(private val size: Int) {

  private val rowArr = new Array[Int](size)
  private val columnArr = new Array[Int](size)

  def add(listPoint: List[Point]): Unit = listPoint.foreach(p => add(p))

  private def add(point: Point): Unit = point match {
    case Point(row, column) =>
      rowArr(row) = rowArr(row) | 1 << (size - 1 - column)
      columnArr(column) = columnArr(column) | 1 << (size - 1 - row)
  }

  def check: Boolean = rowArr.drop(1).zip(rowArr).forall { case (a, b) => (a & b) == 0 } &&
    columnArr.drop(1).zip(columnArr).forall { case (a, b) => (a & b) == 0 }

  def clear(): Unit = {
    util.Arrays.fill(rowArr, 0)
    util.Arrays.fill(columnArr, 0)
  }
}

class Matrix(private val matrix: Array[Array[Byte]]) {
  require(matrix.nonEmpty && matrix.forall(l => l.length == matrix.length))

  val size: Int = matrix.length

  def apply(rowIdx: Int, columnIdx: Int): Byte = matrix(rowIdx)(columnIdx)
}

class Solver(private val matrix: Matrix) {
  type BoardState = List[Point]
  val colorList = List(White, Black)

  /*
  Find coincide number in row or column.
   */
  private val size = matrix.size

  def buildCoincidenceList: List[(Map[Byte, List[Byte]], Map[Byte, List[Byte]])] =
    (for (i <- 0 until size; j <- 0 until size) yield (matrix(i, j), (i, j)))
    /*
    Group by value
     */
    .foldLeft(Map.empty[Byte, List[(Byte, Byte)]]) {
      case (acc, (value, (row, column))) =>
        acc + ((value, (row.toByte, column.toByte) :: acc.getOrElse(value, List.empty[(Byte, Byte)])))
    }
    /*
    Transform to Map Row -> List[Column] and Column -> List[Row]
     */
    .map {
      case (value, list) => (
        value,
        list.foldLeft(Map.empty[Byte, List[Byte]], Map.empty[Byte, List[Byte]]) {
          case ((rowMap, columnMap), (rowIdx, columnIdx)) =>
            (
              rowMap + ((rowIdx, columnIdx :: rowMap.getOrElse(rowIdx, List.empty[Byte]))),
              columnMap + ((columnIdx, rowIdx :: columnMap.getOrElse(columnIdx, List.empty[Byte])))
            )
        }
      )
    }.toList
    /*
    Filter only coincidence case
     */
    .map {
      case (_, (rowMap, columnMap)) =>
        (
          rowMap.filter { case (_, columnIdxList) => columnIdxList.length > 1 },
          columnMap.filter { case (_, rowIdxList) => rowIdxList.length > 1 }
        )
    }
    .filter { case (rowMap, columnMap) => rowMap.nonEmpty || columnMap.nonEmpty }

  /*
  Build Graph list
   */
  def buildGraphMap(list: List[(Map[Byte, List[Byte]], Map[Byte, List[Byte]])]): List[(HeadNode, Set[Point])] = {
    list.flatMap {
      case (rowMap, columnMap) =>
        val pointList = (
          rowMap.flatMap { case (row, listColumn) => listColumn.map(Point(row, _)) } ++
            columnMap.flatMap { case (column, listRow) => listRow.map(Point(_, column)) }
          ).toSet.toList.sorted

        buildGraph(pointList, rowMap, columnMap)
    }
  }

  def buildGraph(
                  pointList: List[Point],
                  rowMap: Map[Byte, List[Byte]],
                  columnMap: Map[Byte, List[Byte]]
                ): Map[HeadNode, Set[Point]] = {

    def buildColumnGraph(
                          columnPointList: List[Point],
                          crossPointSet: Set[Point],
                          remainingPointSet: Set[Point]
                        ): (List[ColumnNode], Set[Point], Set[Point]) = columnPointList

      .foldLeft((List.empty[ColumnNode], crossPointSet, remainingPointSet)) {
        case ((graphListAcc, crossPointSetAcc, remainingPointSetAcc), point@Point(row, column)) =>

          if (remainingPointSetAcc.contains(point)) {

            val rowPointList = rowMap.getOrElse(row, List.empty).filter(_ != column).map(Point(row, _))

            val rowGraphTuple = buildRowGraph(rowPointList, crossPointSetAcc, remainingPointSetAcc - point)

            val newGraph = ColumnNode(point, rowGraphTuple._1)

            (newGraph :: graphListAcc, rowGraphTuple._2, rowGraphTuple._3)
          }
          else (graphListAcc, crossPointSetAcc + point, remainingPointSetAcc)
      }

    def buildRowGraph(
                       rowPointList: List[Point],
                       crossPointSet: Set[Point],
                       remainingPointSet: Set[Point]
                     ): (List[RowNode], Set[Point], Set[Point]) = rowPointList
      .foldLeft((List.empty[RowNode], crossPointSet, remainingPointSet)) {
        case ((graphListAcc, crossPointSetAcc, remainingPointSetAcc), point@Point(row, column)) =>

          if (remainingPointSetAcc.contains(point)) {

            val columnPointList = columnMap.getOrElse(column, List.empty).filter(_ != row).map(Point(_, column))

            val rowGraphTuple = buildColumnGraph(columnPointList, crossPointSetAcc, remainingPointSetAcc - point)

            val newGraphListAcc = RowNode(point, rowGraphTuple._1) :: graphListAcc

            (newGraphListAcc, rowGraphTuple._2, rowGraphTuple._3)
          }
          else (graphListAcc, crossPointSetAcc + point, remainingPointSet)
      }

    def buildGraphRec(
                       graphList: List[HeadNode],
                       crossPointMap: Map[HeadNode, Set[Point]],
                       pointList: List[Point]
                     ): (List[HeadNode], Map[HeadNode, Set[Point]], List[Point]) =
      pointList match {
        case (rootPoint@Point(row, column)) :: xs =>

          val columnPointList = (columnMap.getOrElse(column, List.empty).map(Point(_, column)).toSet - rootPoint).toList

          val columnGraphTuple = buildColumnGraph(columnPointList: List[Point], Set.empty[Point], xs.toSet)

          val rowPointList = (rowMap.getOrElse(row, List.empty).map(Point(row, _)).toSet - rootPoint).toList

          val rowGraphTuple = buildRowGraph(rowPointList: List[Point], columnGraphTuple._2, columnGraphTuple._3)

          val head = HeadNode(rootPoint, columnGraphTuple._1, rowGraphTuple._1)

          buildGraphRec(head :: graphList, crossPointMap + ((head, rowGraphTuple._2)), rowGraphTuple._3.toList.sorted)

        case Nil => (graphList, crossPointMap, pointList)
      }

    buildGraphRec(List.empty[HeadNode], Map.empty[HeadNode, Set[Point]], pointList) match {
      case res =>
        (res._1.toSet -- res._2.keySet).map((_, Set.empty[Point])).toMap ++ res._2
    }
  }

  private def generate(color: Color, graph: Node): List[BoardState] = {

    val (childRowNodeList, childColumnNodeList) = graph match {
      case row: RowNode => (List.empty, row.columnList)
      case column: ColumnNode => (column.rowList, List.empty)
      case head: HeadNode => (head.rowList, head.columnList)
    }
    val totalRes = color match {
      case White =>
        combineVariants((childRowNodeList ++ childColumnNodeList).map(g => generate(Black, g)))

      case Black =>
        val res = combineVariants(
          processList(childRowNodeList).map {
            case (whiteRowNode, blackRowNodeList) =>
              generate(White, whiteRowNode) ++ blackRowNodeList.flatMap(blackRowNode => generate(Black, blackRowNode))
          } ++
          processList(childColumnNodeList).map {
            case (whiteColumnNode, blackColumnNodeList) =>
              generate(White, whiteColumnNode) ++ blackColumnNodeList.flatMap(blackColumnNode => generate(Black, blackColumnNode))
          }
        ) match {
          case boardList@ _ :: _ => boardList.map((list: BoardState) => graph.point :: list)
          case List(Nil) => List(graph.point :: Nil)
          case Nil => List(graph.point :: Nil)
        }
        res
    }
    totalRes
  }

  private def calculateGraphArrangement(graphMap: List[(HeadNode, Set[Point])]): collection.mutable.Map[(HeadNode, Color), List[BoardState]] = {
    val res = collection.mutable.HashMap[(HeadNode, Color), List[BoardState]]()
    colorList.foreach(color =>
      graphMap.foreach {
        case (head, _) =>
          val variants = generate(color, head)
          res.put((head, color), variants)
      }
    )
    res
  }

  def generateBoard(graphMap: List[(HeadNode, Set[Point])]): List[BoardState] = {

    val map: collection.mutable.Map[(HeadNode, Color), List[BoardState]] = calculateGraphArrangement(graphMap)

    val res = graphMap.indices.foldRight(List.empty: List[List[Color]]) {
      case (_, Nil) => colorList.map(_ :: Nil)
      case (_, acc) => colorList.flatMap(color => acc.map(color :: _))
    }
    .flatMap(colorVariant =>
      combineVariants(graphMap.zip(colorVariant).map {
        case ((rootGraph, _), color) => map((rootGraph, color))
      })
    )
    res
  }

  def combineVariants(listOfVariants: List[List[BoardState]]): List[BoardState] = {
    val bitRepr = new BitBoardRepresentation(size)
    listOfVariants
      .foldLeft(List.empty: List[BoardState]) {
        case (Nil, stateList) => stateList
        case (stateAcc, Nil) => stateAcc
        case (stateAcc, stateList) =>
          stateList.flatMap((stateFromAcc: BoardState) =>
            stateAcc
              .map( (stateFromStateList: BoardState) => stateFromStateList ++ stateFromAcc )
              .filter((state: BoardState) => {
                bitRepr.add(state)
                val check = bitRepr.check
                bitRepr.clear()
                check
              })
            )
          }
      }

  def processList(graphList: List[Node]): List[(Node, List[Node])] = graphList
    .map(l => (l, (graphList.toSet - l).toList))

  def checkFullConsistency(list: List[(Map[Byte, List[Byte]], Map[Byte, List[Byte]])], boardState: List[Point]): Boolean = {
    val boardSet = boardState.toSet
    list.map {
      case (rowMap, columnMap) =>
        rowMap.map {
          case (row, columnList) =>
            columnList.map(Point(row, _)).foldLeft(0)((acc, point) => if(boardSet.contains(point)) acc+1 else acc) == 1
        }.forall(x => x) &&
        columnMap.map {
          case (column, rowList) =>
            rowList.map(Point(_, column)).foldLeft(0)((acc, point) => if(boardSet.contains(point)) acc+1 else acc) == 1
        }.forall(x => x)
    }.forall(x => x)
  }

  def checkSimpleConnectivity(boardState: List[Point]): Boolean = {
    val boardSet = boardState.toSet

    val poleEdges: Set[UndirectedEdge] = boardState.filter(point =>
      point.row == 0 || point.row == size - 1 || point.column == 0 || point.column == size - 1
    )
      .foldLeft(Set.empty: Set[UndirectedEdge])((acc, point) =>
        acc + new UndirectedEdge(point, Pole)
      )

    val totalEdges: Set[UndirectedEdge] = boardState.foldLeft(poleEdges) { case (acc, pivotPoint@Point(row, column)) =>
      List(Point((row + 1).toByte, (column + 1).toByte), Point((row + 1).toByte, (column - 1).toByte))
        .filter(boardSet.contains)
        .foldLeft(acc)((acc, point) => acc + new UndirectedEdge(pivotPoint, point))
    }
    val connectedEdgeMap: Map[SuperPoint, Set[SuperPoint]] = totalEdges.foldLeft(Map.empty: Map[SuperPoint, Set[SuperPoint]])((acc, edge) =>
      acc + ((edge.point1, acc.getOrElse(edge.point1, Set.empty) + edge.getOtherVertex(edge.point1)))
        + ((edge.point2, acc.getOrElse(edge.point2, Set.empty) + edge.getOtherVertex(edge.point2)))
    )

    def next(curPoint: SuperPoint, nextPoint: SuperPoint, processedPoints: Set[SuperPoint], res: Boolean): (Set[SuperPoint], Boolean) = {
      val nextPoints = connectedEdgeMap(nextPoint) - curPoint
      nextPoints.foldLeft((processedPoints + nextPoint, res)) {
        case ((_, false), _) => (Set.empty, false)
        case ((processedPointsAcc, true), point) =>
          if (processedPointsAcc.contains(point)) (Set.empty, false)
          else next(nextPoint, point, processedPointsAcc, res = true)
      }
    }

    val allPoints = connectedEdgeMap.keys.toList
    allPoints.foldLeft((Set.empty: Set[SuperPoint], true)) {
      case ((_, false),_) => (Set.empty, false)
      case ((setAcc, true), point) =>
        if(setAcc.contains(point)) (setAcc, true)
        else {
          val nextPoints = connectedEdgeMap(point)
          nextPoints.foldLeft((setAcc + point, true)) {
            case ((set, res), nextPoint) => next(point, nextPoint, set, res)
          }
        }
    } match {
      case (_, true) => true
      case _ => false
    }
  }

  def outputPoint(res: List[Point]): Unit = {
    val set = res.toSet
    (for (
      i <- 0 until size;
      j <- 0 until size
    ) yield Point(i.toByte, j.toByte)).foreach(
      point => {
        if (set.contains(point)) print(" 1 ")
        else print(" 0 ")
        if (point.column == size - 1) println()
      }
    )
    println()
  }
}

