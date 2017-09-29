package com.solver

import java.util

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

case class Point(row: Byte, column: Byte) extends Ordered[Point] {
  import scala.math.Ordered.orderingToOrdered

  def compare(that: Point): Int = (this.row, this.column) compare (that.row, that.column)
}

sealed trait Graph { val point: Point }
sealed trait RowTrait extends Graph
sealed trait ColumnTrait extends Graph
case class Row(override val point: Point, columnList: List[Column]) extends RowTrait
case class Column(override val point: Point, rowList: List[Row]) extends ColumnTrait
case class Head(override val point: Point, columnList: List[Column], rowList: List[Row]) extends RowTrait with ColumnTrait

sealed trait Color
case object White extends Color
case object Black extends Color

class BitBoardRepresentation(private val size: Int) {

  private val rowArr = new Array[Int](size)
  private val columnArr = new Array[Int](size)

  private def add(point: Point): Unit = point match {
    case Point(row, column) =>
      rowArr(row) = rowArr(row) |  1 << (size-1-column)
      columnArr(column) = columnArr(column) |  1 << (size-1-row)
  }

  def add(listPoint: List[Point]): Unit = listPoint.foreach(p => add(p))

  def check: Boolean = rowArr.drop(1).zip(rowArr).forall { case (a,b) => (a & b) == 0 } &&
    columnArr.drop(1).zip(columnArr).forall { case (a,b) => (a & b) == 0 }

  def clear(): Unit = {
    util.Arrays.fill(rowArr, 0)
    util.Arrays.fill(columnArr, 0)
  }
}

class Matrix(private val matrix: Array[Array[Byte]]) {
  require(matrix.nonEmpty && matrix.forall(l => l.length == matrix.length))

  val size: Int = matrix.length

  def get(rowIdx: Int): Vector[Byte] = matrix(rowIdx).toVector

  def get(rowIdx: Int, columnIdx: Int): Byte = matrix(rowIdx)(columnIdx)
}

class Solver(private val matrix: Matrix) {
  private val size = matrix.size

  /*
  Find coincide number in row or column.
   */

  def buildGraphMap: Map[Head, Set[Point]] = (for (i <- 0 until size; j <- 0 until size ) yield
    (matrix.get(i, j), (i, j)))
    /*
    Group by value
     */
    .foldLeft(Map.empty[Byte,List[(Byte,Byte)]]) {
      case(acc, (value, (row, column))) =>
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
    }
    /*
    Filter only coincidence case
     */
    .map {
      case(_, (rowMap, columnMap)) =>
        (
          rowMap.filter { case (_, columnIdxList) => columnIdxList.length > 1 },
          columnMap.filter { case (_, rowIdxList) => rowIdxList.length > 1 }
        )
    }
    .filter { case(rowMap, columnMap) => rowMap.nonEmpty || columnMap.nonEmpty }
    /*
    Build Graph list
     */
    .flatMap {
      case(rowMap, columnMap) =>
        val pointList = (
          rowMap.flatMap { case (row, listColumn) => listColumn.map(Point(row, _)) } ++
          columnMap.flatMap { case (column, listRow) => listRow.map(Point(_, column)) }
        ).toSet.toList.sorted
        println(pointList)
        println()
        buildGraph(pointList, rowMap, columnMap)
    }.toMap

  def buildGraph(
                  pointList: List[Point],
                  rowMap: Map[Byte, List[Byte]],
                  columnMap: Map[Byte, List[Byte]]
                ): Map[Head, Set[Point]] = {

    def buildColumnGraph(
                          columnPointList: List[Point],
                          crossPointSet: Set[Point],
                          remainingPointSet: Set[Point]
                        ): (List[Column], Set[Point], Set[Point]) = columnPointList

      .foldLeft((List.empty[Column], crossPointSet, remainingPointSet)) {
        case ((graphListAcc, crossPointSetAcc, remainingPointSetAcc), point @ Point(row, column)) =>

          if(remainingPointSetAcc.contains(point)) {

            val rowPointList = rowMap.getOrElse(row, List.empty).filter(_ != column).map(Point(row, _))

            val rowGraphTuple = buildRowGraph(rowPointList, crossPointSetAcc, remainingPointSetAcc - point)

            val newGraph = Column(point, rowGraphTuple._1)

            (newGraph :: graphListAcc, rowGraphTuple._2, rowGraphTuple._3)
          }
          else (graphListAcc, crossPointSetAcc + point, remainingPointSetAcc)
      }

    def buildRowGraph(
                       rowPointList: List[Point],
                       crossPointSet: Set[Point],
                       remainingPointSet: Set[Point]
                     ): (List[Row], Set[Point], Set[Point]) = rowPointList
      .foldLeft((List.empty[Row], crossPointSet, remainingPointSet)) {
        case ((graphListAcc, crossPointSetAcc, remainingPointSetAcc), point @ Point(row, column)) =>

          if(remainingPointSetAcc.contains(point)) {

            val columnPointList = columnMap.getOrElse(column, List.empty).filter(_ != row).map(Point(_, column))

            val rowGraphTuple = buildColumnGraph(columnPointList, crossPointSetAcc, remainingPointSetAcc - point)

            val newGraphListAcc = Row(point, rowGraphTuple._1) :: graphListAcc

            (newGraphListAcc, rowGraphTuple._2, rowGraphTuple._3)
          }
          else (graphListAcc, crossPointSetAcc + point, remainingPointSet)
      }

    def buildGraphRec(
                       graphList: List[Head],
                       crossPointMap: Map[Head, Set[Point]],
                       pointList: List[Point]
                     ): (List[Head], Map[Head, Set[Point]], List[Point]) =
      pointList match {
        case (rootPoint @ Point(row, column)) :: xs =>

          val columnPointList = (columnMap.getOrElse(column, List.empty).map(Point(_, column)).toSet - rootPoint).toList

          val columnGraphTuple = buildColumnGraph(columnPointList: List[Point], Set.empty[Point], xs.toSet)

          val rowPointList = (rowMap.getOrElse(row, List.empty).map(Point(row, _)).toSet - rootPoint).toList

          val rowGraphTuple = buildRowGraph(rowPointList: List[Point], columnGraphTuple._2, columnGraphTuple._3)

          val head = Head(rootPoint, columnGraphTuple._1, rowGraphTuple._1)

          buildGraphRec(head :: graphList, crossPointMap + ((head, rowGraphTuple._2)), rowGraphTuple._3.toList.sorted)

        case Nil => (graphList, crossPointMap, pointList)
      }

      buildGraphRec(List.empty[Head], Map.empty[Head, Set[Point]], pointList) match {
        case res =>
          (res._1.toSet -- res._2.keySet).map((_, Set.empty[Point])).toMap ++ res._2
      }
  }

  def combineVariants(listOfVariants: List[List[List[Point]]]): List[List[Point]] = listOfVariants
    .foldLeft(List.empty: List[List[Point]]) {
      case (acc: List[List[Point]], listElm: List[List[Point]]) =>
        acc match {
          case newAcc @ x :: xs => newAcc.flatMap(accElm => listElm.map( listElmElm => listElmElm ++ accElm))
          case Nil => listElm
        }
    }

  def processList(graphList: List[Graph]): List[(Graph, List[Graph])] = graphList
    .combinations(1).map(l => (l.head, (graphList.toSet -- l.toSet).toList)).toList

  def generateBoard(graphMap: Map[Head, Set[Point]]): List[List[Point]] = {

    def generate(color: Color, graph: Graph): List[List[Point]] = {
      val (childRowNodeList, childColumnNodeList) = graph match {
        case row: Row => (List.empty, row.columnList)
        case column: Column => (column.rowList, List.empty)
        case head: Head => (head.rowList, head.columnList)
      }
      color match {
        case White =>
          (childRowNodeList ++ childColumnNodeList).flatMap(g => generate(Black, g))

        case Black =>

          val rowList: List[List[List[Point]]] = processList(childRowNodeList).map {
            case (whiteRowNode, blackRowNodeList) =>
              generate(White, whiteRowNode) ++ blackRowNodeList.flatMap( blackRowNode => generate(Black, blackRowNode))
          }

          val columnList: List[List[List[Point]]] = processList(childColumnNodeList).map {
            case (whiteColumnNode, blackColumnNodeList) =>
              generate(White, whiteColumnNode) ++ blackColumnNodeList.flatMap( blackColumnNode => generate(Black, blackColumnNode))
          }

          val res: List[List[Point]] = combineVariants(rowList ++ columnList).map(list => graph.point :: list)
          if(res.isEmpty) List(List(graph.point)) else res
      }
    }

    combineVariants(graphMap.keys.map( rootGraph => generate(Black, rootGraph) ++ generate(White, rootGraph)).toList)
  }

  def outputPoint(res: List[Point]): Unit = {
    val set = res.toSet
    (for (
      i <- 0 until size;
      j <- 0 until size
    ) yield Point(i.toByte,j.toByte)).foreach(
      point => {
        if(set.contains(point)) print(" 1 ")
        else print(" 0 ")
        if(point.column == size - 1 ) println()
      }
    )

  }

  /*
  def foldGraph[A](graph: Graph, acc: A)(f: (A, Graph) => A): A = graph match {
    case Row(_, columnList) => columnList.foldLeft(acc)((acc, g) => foldGraph(g, acc)(f))
    case Column(_, rowList) => rowList.foldLeft(acc)((acc, g) => foldGraph(g, acc)(f))
    case Head(_, columnList, rowList) =>
      val newAcc = columnList.foldLeft(acc)((acc, g) => foldGraph(g, acc)(f))
      rowList.foldLeft(newAcc)((acc, g) => foldGraph(g, acc)(f))
  }
   */
}

