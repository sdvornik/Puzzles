package com.solver

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

object Solver extends App {

  class Matrix(private val matrix: Array[Array[Byte]]) {
    require(matrix.nonEmpty && matrix.forall(l => l.length == matrix.length))

    def get(rowIdx: Int): Vector[Byte] = matrix(rowIdx).toVector

    def get(rowIdx: Int, columnIdx: Int): Byte = matrix(rowIdx)(columnIdx)

    def transpose: Matrix = {
      val newMatrix: Array[Array[Byte]] = Array.fill(matrix.length, matrix.length)(0.toByte)
      for(i <- matrix.indices; j <- matrix.indices ) newMatrix(i)(j) = matrix(j)(i)
      new Matrix(newMatrix)
    }
  }

  /*
  Find coincide number in row or column. Return indexes of this elements grouped in list.
   */
  def findCoincidence(line: Vector[Byte]): List[List[Int]] = line
    .zipWithIndex
    .foldLeft(Map.empty[Byte,List[Int]]) {
      case(acc, (value, index)) => acc + ((value, index :: acc.getOrElse(value, List.empty[Int])))
    }
    .toList
    .filter { case(_, list) => list.length > 1}
    .map { case (_, list) => list }

  def findCoincidence(matrix: Matrix, size: Int) = (for (
    i <- 0 until size;
    j <- 0 until size
  ) yield (matrix.get(i, j), (i, j))).toList.foldLeft(Map.empty[Byte,List[(Byte,Byte)]]) {
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
      rowMap.toList
      null
  }

  /*
  Encode rows or columns in bit representation (1-black, 0-white).
   */
  def findLineNumbers(list: List[List[Int]], size: Int): List[Int] = {

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

  def combineColumnAndRows(columns: List[Int], rows: List[Int], size: Int): (List[Int], List[Int]) = {

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

  /*
  Program body
   */
  val puzzle: Array[Array[Byte]] = Array(
    Array(1,1,3,5,5),
    Array(4,2,5,3,1),
    Array(4,3,3,5,4),
    Array(5,1,2,3,3),
    Array(3,3,4,1,1)
  ).map(_.map(_.toByte))

  val matrix = new Matrix(puzzle)
  val matrixT = matrix.transpose
  val size = puzzle.length

  val columns: List[List[Int]] = (0 until size).map(matrix.get).map(findCoincidence).map(findLineNumbers(_,size)).toList
  println(columns)
  val rows: List[List[Int]] = (0 until size).map(matrixT.get).map(findCoincidence).map(findLineNumbers(_,size)).toList
  println(rows)

  val boards: List[(List[Int], List[Int])] = generateBoard(columns, rows)



  boards.map {
    case(columnsV, rowsV) => combineColumnAndRows(columnsV, rowsV, size)
  }.map(checkBoard)


}

