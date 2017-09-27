package com.solver

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

case class Point(row: Byte, column: Byte) extends Ordered[Point] {
  import scala.math.Ordered.orderingToOrdered

  def compare(that: Point): Int = (this.row, this.column) compare (that.row, that.column)
}

sealed trait Graph
sealed trait RowTrait extends Graph
sealed trait ColumnTrait extends Graph
case class Row(point: Point, columnList: List[Column]) extends RowTrait
case class Column(point: Point, rowList: List[Row]) extends ColumnTrait
case class Head(point: Point, columnList: List[Column], rowList: List[Row]) extends RowTrait with ColumnTrait


class Solver(private val matrix: Matrix) {
  private val size = matrix.size

  /*
  Find coincide number in row or column.
   */

  def buildGraphList: List[Graph] = (for (i <- 0 until size; j <- 0 until size ) yield
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
      case(value, (rowMap: Map[Byte, List[Byte]], columnMap: Map[Byte, List[Byte]])) =>
        (
          value,
          rowMap.filter { case (_, columnIdxList) => columnIdxList.length > 1 },
          columnMap.filter { case (_, rowIdxList) => rowIdxList.length > 1 }
        )
    }
    /*
    Build Graph list
     */
    .flatMap {
      case(value, rowMap: Map[Byte, List[Byte]], columnMap: Map[Byte, List[Byte]]) =>
        val pointSet = rowMap.toList.flatMap { case (row, listColumn) => listColumn.map(Point(row, _)) }.toSet ++
        columnMap.toList.flatMap { case (column, listRow) => listRow.map(Point(_, column)) }.toSet
        println()
        println(s"____________________________Value: $value")

        println(pointSet.toList.sorted)
        val res = buildGraph(pointSet.toList.sorted, rowMap, columnMap)
       println(res)
      res
    }
    .toList

  def buildGraph(pointList: List[Point], rowMap: Map[Byte, List[Byte]], columnMap: Map[Byte, List[Byte]]): List[Graph] = {

    def buildColumnGraph(columnPointList: List[Point], remainingPointSet: Set[Point]): (List[Column], Set[Point]) = columnPointList
      .foldLeft((List.empty[Column], remainingPointSet)) {
        case ((graphListAcc, remainingPointSetAcc), point @ Point(row, column)) =>
          println(s"Call buildColumnGraph at point $point")

          val newRemainingPointSet = remainingPointSetAcc - point

          val rowPointList = rowMap.getOrElse(row, List.empty).map(Point(row, _)).toSet.intersect(newRemainingPointSet).toList

          println(s"rowPointList $rowPointList")
          val rowGraphTuple = buildRowGraph(rowPointList, newRemainingPointSet)


          val newGraph = Column(point, rowGraphTuple._1)

          val points: List[Column] = foldGraph(newGraph, List.empty[Column]){
            case (acc, graph) =>
              // compare point to graph points
              null
          }
          val newGraphListAcc = newGraph :: graphListAcc

          (newGraphListAcc, rowGraphTuple._2)

      }

    def buildRowGraph(rowPointList: List[Point], remainingPointSet: Set[Point]): (List[Row], Set[Point]) = rowPointList
      .foldLeft((List.empty[Row], remainingPointSet)) {
        case ((graphListAcc, remainingPointSetAcc), point @ Point(row, column)) =>
          println(s"Call buildRowGraph at point $point")

          val newRemainingPointSet = remainingPointSetAcc - point

          val columnPointList = columnMap.getOrElse(column, List.empty).map(Point(_, column)).toSet.intersect(newRemainingPointSet).toList

          println(s"columnPointList $columnPointList")
          val rowGraphTuple = buildColumnGraph(columnPointList, newRemainingPointSet)

          val newGraphListAcc = Row(point, rowGraphTuple._1) :: graphListAcc

          (newGraphListAcc, rowGraphTuple._2)

      }

    def buildGraphRec(graphList: List[Graph], pointList: List[Point]): (List[Graph], List[Point]) = pointList match {

      case (rootPoint @ Point(row, column)) :: xs =>
        val remainingPointSet = xs.toSet
        println()
        println(s"********************Execute buildGraphRec at point $rootPoint")
        val rowPointList = (rowMap.getOrElse(row, List.empty).map(Point(row, _)).toSet - rootPoint).toList

        val columnPointList = (columnMap.getOrElse(column, List.empty).map(Point(_, column)).toSet - rootPoint).toList

        val columnGraphTuple: (List[Column], Set[Point]) = buildColumnGraph(columnPointList: List[Point], remainingPointSet)
        val rowGraphTuple: (List[Row], Set[Point]) = buildRowGraph(rowPointList: List[Point], columnGraphTuple._2)

        val newGraphList = Head(rootPoint, columnGraphTuple._1, rowGraphTuple._1) :: graphList
        println(s"********************Build head graph $newGraphList")
        buildGraphRec(newGraphList, rowGraphTuple._2.toList.sorted)

      case Nil => (graphList, pointList)
    }
    buildGraphRec(List.empty[Graph], pointList)._1
  }

  def traverseGraph(graph: Graph): Unit = {
    graph match {
      case Row(point, columnList) => columnList.foreach(traverseGraph)
      case Column(point, rowList) => rowList.foreach(traverseGraph)
      case Head(point, columnList, rowList) =>
        columnList.foreach(traverseGraph)
        rowList.foreach(traverseGraph)
    }
  }

  def foldGraph[A](graph: Graph, acc: A)(f: (A, Graph) => A): A = graph match {
    case Row(point, columnList) => columnList.foldLeft(acc)((acc, g) => foldGraph(g, acc)(f))
    case Column(point, rowList) => rowList.foldLeft(acc)((acc, g) => foldGraph(g, acc)(f))
    case Head(point, columnList, rowList) =>
      val newAcc = columnList.foldLeft(acc)((acc, g) => foldGraph(g, acc)(f))
      rowList.foldLeft(newAcc)((acc, g) => foldGraph(g, acc)(f))
  }


  /*
  Encode rows or columns in bit representation (1-black, 0-white).
   */
  def findLineNumbers(list: List[List[Int]]): List[Int] = {

    def findLineNumbersRec(list: List[List[Int]], acc: List[Int]): List[Int] = list match {
      case x::xs =>
        val newAcc: List[Int] = acc.flatMap(accElm =>
          x.map(v => 1 << (size-1-v) | accElm )
        )
        findLineNumbersRec(xs, newAcc)
      case Nil => acc
    }
    findLineNumbersRec(list, List(0))
  }

  def generateBoard(columnsVariants: List[List[Int]], rowsVariants: List[List[Int]]): List[(List[Int], List[Int])] = {

    def generate(list: List[List[Int]], acc: List[List[Int]]): List[List[Int]] = list match {
      case head::tail =>
        val newAcc: List[List[Int]] = head.flatMap(l => acc.map(x => l::x))
        generate(tail, newAcc)
      case Nil => acc
    }

    val column = generate(columnsVariants, List(List.empty[Int]))
    val row = generate(rowsVariants, List(List.empty[Int]))

    column.flatMap( (x: List[Int]) => row.map((y: List[Int]) => (x,y)))
  }

  def combineColumnAndRows(columns: List[Int], rows: List[Int]): (List[Int], List[Int]) = {

    val matrixList: List[(Byte, Byte, Boolean)] = columns.zipWithIndex
      .foldLeft(List.empty[(Byte, Byte, Boolean)]) { case (acc, (columnElm, idxColumn)) =>
        rows.zipWithIndex.foldLeft(acc) { case (a, (rowElm, idxRow)) =>
          (idxColumn.toByte, idxRow.toByte, (1 << (size-1-idxRow) & columnElm) != 0 || (1 << (size-1-idxColumn) & rowElm) != 0) :: a
        }
      }

    matrixList
      .foldLeft((Map.empty[Byte, List[(Byte, Boolean)]], Map.empty[Byte, List[(Byte, Boolean)]])) {
        case (acc, (idxColumn, idxRow, bitValue) ) =>
          val columnMap = acc._1
          val rowMap = acc._2
          (
            columnMap + ((idxColumn, (idxRow, bitValue) :: columnMap.getOrElse(idxColumn, List.empty[(Byte, Boolean)]))),
            rowMap + ((idxRow, (idxColumn, bitValue) :: rowMap.getOrElse(idxRow, List.empty[(Byte, Boolean)])))
          )
      } match {
      case (columnMap, rowMap) => (
        columnMap.toList.sortBy { case (id, _) => id }.map { case (_, list) => list.foldLeft(0) {
          case (acc,(idx, isValue)) =>
            if(isValue) acc | (1 << (size-1-idx)) else acc
        }},
        rowMap.toList.sortBy { case (id, _) => id }.map { case (_, list) => list.foldLeft(0) {
          case (acc,(idx, isValue)) =>
            if(isValue) acc | (1 << (size-1-idx)) else acc
        }}
      )
    }
  }

  def checkLines(linesList: List[Int]): Boolean = linesList.zip(linesList.drop(1)).forall { case(first, second) => (first & second) == 0 }

  def checkBoard(board: (List[Int], List[Int])): Boolean = board match {
    case (columnList, rowList) =>
      if(columnList.zip(columnList.drop(1)).forall { case(first, second) => (first & second) == 0 } &&
        rowList.zip(rowList.drop(1)).forall { case(first, second) => (first & second) == 0 }) { output(columnList); true}
      else false
  }

  def output(res: List[Int]): Unit = {
    (0 until size).foreach(i => {
      val number = res(i)
      (0 until size).foreach(k => if(((1 << (size-1-k))& number) == 0) print(0+" ") else print(1+" ") )
      println()
    })
  }




}

