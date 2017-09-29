package com.solver

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
      case(value, (rowMap, columnMap)) =>
        (
          value,
          rowMap.filter { case (_, columnIdxList) => columnIdxList.length > 1 },
          columnMap.filter { case (_, rowIdxList) => rowIdxList.length > 1 }
        )
    }
    .filter { case(value, rowMap, columnMap) => rowMap.nonEmpty || columnMap.nonEmpty }
    /*
    Build Graph list
     */
    .flatMap {
      case(value, rowMap, columnMap) =>
        val pointList = (
          rowMap.flatMap { case (row, listColumn) => listColumn.map(Point(row, _)) } ++
          columnMap.flatMap { case (column, listRow) => listRow.map(Point(_, column)) }
        ).toSet.toList.sorted
        println(s"Value: $value ____________________________")
        println(pointList)

        val res = buildGraph(pointList, rowMap, columnMap)
        println()
        res
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
    case Row(_, columnList) => columnList.foldLeft(acc)((acc, g) => foldGraph(g, acc)(f))
    case Column(_, rowList) => rowList.foldLeft(acc)((acc, g) => foldGraph(g, acc)(f))
    case Head(_, columnList, rowList) =>
      val newAcc = columnList.foldLeft(acc)((acc, g) => foldGraph(g, acc)(f))
      rowList.foldLeft(newAcc)((acc, g) => foldGraph(g, acc)(f))
  }

  def combineVariants(listOfVariants: List[List[List[Point]]]): List[List[Point]] = listOfVariants
    .foldLeft(List.empty: List[List[Point]]) {
      case (acc: List[List[Point]], listElm: List[List[Point]]) =>
        acc match {
          case x :: xs => for(
            list: List[Point] <- listElm;
            accList: List[Point] <- acc
          ) yield accList ++ list
          case Nil => listElm
        }
    }

  def generateBoard(graphMap: Map[Head, Set[Point]]): List[List[Point]] = {

    def generate(color: Color, graph: Graph): List[List[Point]] = {

      val childNodeList = graph match {
        case row: Row => (List.empty, row.columnList)
        case column: Column => (column.rowList, List.empty)
        case head: Head => (head.rowList, head.columnList)
      }
      color match {
        case White =>
          val blackList: List[Graph] = childNodeList._1 ++ childNodeList._2

          println(s"BLACK_LIST ${blackList.map(_.point)}___________________________________")

          var res: List[List[Point]] = combineVariants(blackList.map(g => generate(Black, g)))

          println(s"BLACK OUTPUT:\n$res")
          res

        case Black =>
          def processList(graphList: List[Graph]): List[(Graph, List[Graph])] = graphList
            .combinations(1).map(l => (l.head, (graphList.toSet -- l.toSet).toList)).toList

          val allPossibilities: List[((Graph, List[Graph]), (Graph, List[Graph]))] = for (
            rowGen <- processList(childNodeList._1);
            columnGen <- processList(childNodeList._2)
          ) yield (rowGen, columnGen)

          println(s"allPossibilities $allPossibilities")

          val res: List[List[Point]] = allPossibilities.flatMap {
            case ((whiteRowNode, blackRowNodeList), (whiteColumnNode, blackColumnNodeList)) =>

              val blackList: List[Graph] = blackRowNodeList ++ blackColumnNodeList

              blackList.flatMap(g => blackList.map(_.point) :: (generate(Black, g) ++ generate(White, whiteRowNode) ++ generate(White, whiteColumnNode)))

          }
          println(s"WHITE OUTPUT:\n$res")
          res

      }
    }

    combineVariants(graphMap.keys.map( rootGraph =>
          combineVariants(List(generate(Black, rootGraph), generate(White, rootGraph)))
    ).toList)

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

  def outputPoint(res: List[Point]): Unit = {
    val set = res.toSet
    (for (
      i <- 0 until size;
      j <- 0 until size
    ) yield Point(i.toByte,j.toByte)).foreach(
      point => {
        if(set.contains(point)) print(" 1 ")
        else print(" 0 ")
        if(point.column == size -1 ) println()
      }
    )

  }
}

