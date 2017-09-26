package com.solver

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
sealed trait Direction
case object Row extends Direction
case object Column extends Direction

case class Point(row: Byte, column: Byte)
case class Edge(point1: Point, point2: Point, direction: Direction)

sealed trait Tree
case class Leaf(value: Point) extends Tree
case class Branch(row: List[Point], column: List[Point]) extends Tree


class Solver(private val matrix: Matrix) {
  private val size = matrix.size

  /*
  Find coincide number in row or column.
   */

  def findCoincidence(): Unit = (for (i <- 0 until size; j <- 0 until size ) yield
    (matrix.get(i, j), (i, j)))
    .foldLeft(Map.empty[Byte,List[(Byte,Byte)]]) {
      case(acc, (value, (row, column))) =>
        acc + ((value, (row.toByte, column.toByte) :: acc.getOrElse(value, List.empty[(Byte, Byte)])))
    }.map {
      case (value, list) => (
        value,
        list.foldLeft(Map.empty[Byte, List[Byte]], Map.empty[Byte, List[Byte]]) {
          case ((rowMap, columnMap), (rowIdx, columnIdx)) =>
            val newRowMap = rowMap + ((rowIdx, columnIdx :: rowMap.getOrElse(rowIdx, List.empty[Byte])))
            val newColumnMap = columnMap + ((columnIdx, rowIdx :: columnMap.getOrElse(columnIdx, List.empty[Byte])))
            (newRowMap, newColumnMap)
        }
      )
    }.map {
      case(value, (rowMap: Map[Byte, List[Byte]], columnMap: Map[Byte, List[Byte]])) =>
        val filteredRowMap = rowMap.filter {
          case (_, columnIdxList) => columnIdxList.length > 1
        }
        val filteredColumnMap = columnMap.filter {
          case (_, rowIdxList) => rowIdxList.length > 1
        }
        val rowList = filteredRowMap.toList.sortBy {
          case (rowIdx, columnIdxList) => rowIdx
        }
        println(s"Value: $value")
        println(rowList.mkString("\n"))
        //println(filteredColumnMap)
        (filteredRowMap, filteredColumnMap)
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

