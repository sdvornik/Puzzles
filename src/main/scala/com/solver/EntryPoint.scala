package com.solver

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
object EntryPoint extends App {

  val puzzle12: Array[Array[Byte]] = Array(
    Array( 6,  4,  1,  4,  6, 11, 12, 10,  9,  5,  9,  5),
    Array( 2,  9,  3,  7,  8,  7,  1,  4, 10,  7,  6,  3),
    Array( 8,  4, 10,  3, 11,  6,  4,  5,  9,  2,  6,  9),
    Array(10,  5,  2,  6, 12,  7,  2,  9,  3, 11,  8,  4),
    Array( 1,  4,  7, 12,  7,  8,  2,  3,  2,  1,  4, 10),
    Array( 5, 10,  3,  7,  6,  4,  5, 12,  1,  7,  2,  9),
    Array( 4,  6,  5, 11,  7,  4,  9,  2,  2, 12,  3,  1),
    Array( 1,  2, 12, 10,  7,  3,  6,  8,  9,  5,  4, 11),
    Array( 3,  8,  7,  3, 11,  2,  7,  1,  2, 10, 11,  6),
    Array(12,  8,  2,  2, 10,  9,  5, 10,  6,  3,  1, 11),
    Array(11,  6,  8,  1,  7, 12, 10, 11,  3,  7,  6,  2),
    Array( 1, 11,  2,  8,  2, 11,  3,  6,  7,  4, 10,  2)
  ).map(_.map(_.toByte))

  val puzzle5: Array[Array[Byte]] = Array(
    Array(1,1,3,5,5),
    Array(4,2,5,3,1),
    Array(4,3,3,5,4),
    Array(5,1,2,3,3),
    Array(3,3,4,1,1)
  ).map(_.map(_.toByte))


  /*
  Program body
 */
  val matrix = new Matrix(puzzle12)
  val solver = new Solver(matrix)
  solver.findCoincidence()
  /*
  val matrix = new Matrix(puzzle5)
  val matrixT = matrix.transpose
  val size = matrix.size

  val solver = new Solver(matrix)
  import solver._

  val columns: List[List[Int]] = (0 until size).map(matrix.get).map(findCoincidence).map(findLineNumbers(_,size)).toList
  println(columns)
  val rows: List[List[Int]] = (0 until size).map(matrixT.get).map(findCoincidence).map(findLineNumbers(_,size)).toList
  println(rows)

  val boards: List[(List[Int], List[Int])] = generateBoard(columns, rows)



  boards.map {
    case(columnsV, rowsV) => combineColumnAndRows(columnsV, rowsV, size)
  }.map(checkBoard)
  */
}