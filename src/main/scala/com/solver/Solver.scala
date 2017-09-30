package com.solver

import java.util

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

  def generateBoard(graphMap: List[(HeadNode, Set[Point])]): List[BoardState] = {

    def generate(color: Color, graph: Node): List[BoardState] = {

      val (childRowNodeList, childColumnNodeList) = graph match {
        case row: RowNode => (List.empty, row.columnList)
        case column: ColumnNode => (column.rowList, List.empty)
        case head: HeadNode => (head.rowList, head.columnList)
      }
      color match {
        case White =>
          combineVariants((childRowNodeList ++ childColumnNodeList).map(g => generate(Black, g)))

        case Black =>
          combineVariants(
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
      }
    }

    val headColorVariants = (0 until graphMap.size).foldRight(List.empty: List[List[Color]]) {
      case (_, Nil) => List(List(Black), List(White))
      case (_, acc) =>
        acc.map(Black :: _) ++ acc.map(White :: _)
    }

    headColorVariants.flatMap(colorList =>
      combineVariants(graphMap.zip(colorList).map {
        case ((rootGraph, _), color) => generate(color, rootGraph)
      })
    )

  }

  def combineVariants(listOfVariants: List[List[BoardState]]): List[BoardState] = {
    val bitRepr = new BitBoardRepresentation(size)
    val res = listOfVariants
      .filter(_.nonEmpty)
      .foldLeft(List.empty: List[BoardState]) {
        case (stateAcc: List[BoardState], stateList: List[BoardState]) =>
          stateAcc match {
            case _ :: _ => stateAcc.flatMap((stateFromAcc: BoardState) => stateList
              .map(
                (stateFromStateList: BoardState) => {
                  stateFromStateList ++ stateFromAcc
                }
              )
              .filter((state: BoardState) => {
                bitRepr.add(state)
                val check = bitRepr.check
                bitRepr.clear()
                check
              })
            )
            case Nil => stateList
          }
      }
    res
  }

  def processList(graphList: List[Node]): List[(Node, List[Node])] = graphList
    .map(l => (l, (graphList.toSet - l).toList))

  def checkConsistency(list: List[(Map[Byte, List[Byte]], Map[Byte, List[Byte]])], boardState: List[Point]): Boolean = {
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
    .foldLeft(Set.empty: Set[UndirectedEdge]) ((acc, point) =>
      acc + new UndirectedEdge(point, Pole)
    )

    val totalEdges: Set[UndirectedEdge] = boardState.foldLeft(poleEdges) { case (acc, pivotPoint @ Point(row, column)) =>
        List(Point((row+1).toByte, (column+1).toByte), Point((row+1).toByte, (column-1).toByte))
        .filter(boardSet.contains)
        .foldLeft(acc) ((acc, point) => acc + new UndirectedEdge(pivotPoint, point))
    }
    val connectedEdgeMap: Map[SuperPoint, Set[UndirectedEdge]] = totalEdges.foldLeft(Map.empty: Map[SuperPoint, Set[UndirectedEdge]]) ((acc, edge) =>
      acc  + ((edge.point1, acc.getOrElse(edge.point1, Set.empty: Set[UndirectedEdge]) + edge))
        + ((edge.point2, acc.getOrElse(edge.point2, Set.empty: Set[UndirectedEdge]) + edge))
    )

    def next( point: SuperPoint, isNotConnected: Boolean, processedPoints: Set[SuperPoint], processedEdges: Set[UndirectedEdge])
    : (Boolean, Set[SuperPoint], Set[UndirectedEdge]) = {
      val edgeSet = connectedEdgeMap.getOrElse(point, Set.empty)
      (edgeSet -- processedEdges)
        .foldLeft((isNotConnected, processedPoints, processedEdges)) {
          case ((false, _, _),_) => (false, Set.empty, Set.empty)
          case ((true, processedPointsAcc, processedEdgesAcc), edge) =>
            val p = edge.getOtherVertex(point)
            if (processedPoints.contains(p)) (false, Set.empty, Set.empty)
            else next(p, isNotConnected = true, processedPointsAcc + point, processedEdgesAcc + edge)
        }
    }

    connectedEdgeMap.keys.foldLeft((true, Set.empty: Set[SuperPoint], Set.empty: Set[UndirectedEdge])) {
      case ((false, _, _), _) => (false, Set.empty, Set.empty)
      case((true, processedPointAcc, processedEdgeAcc), point) =>

        if(processedPointAcc.contains(point)) (true, processedPointAcc, processedEdgeAcc)
        else next(point, isNotConnected = true, processedPointAcc, processedEdgeAcc)
    } match {
      case (true, _, _) => true
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

