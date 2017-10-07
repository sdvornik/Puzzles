package com.hitori




/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
object EntryPoint extends App {

  val puzzle_12: Array[Array[Byte]] = Array(
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

  val puzzle_5_2: Array[Array[Byte]] = Array(
    Array(1,1,3,5,5),
    Array(4,2,5,3,1),
    Array(4,3,3,5,4),
    Array(5,1,2,3,3),
    Array(3,3,4,1,1)
  ).map(_.map(_.toByte))

  val puzzle_5_1: Array[Array[Byte]] = Array(
    Array( 4, 4, 1, 2, 3),
    Array( 1, 1, 5, 4, 4),
    Array( 2, 5, 3, 4, 3),
    Array( 4, 3, 1, 1, 5),
    Array( 1, 1, 4, 5, 4)
  ).map(_.map(_.toByte))


  val puzzle_5_3: Array[Array[Byte]] = Array(
    Array(1,2,3,4,5),
    Array(2,3,4,5,1),
    Array(3,4,5,1,2),
    Array(4,5,1,2,3),
    Array(5,4,3,2,1)
  ).map(_.map(_.toByte))

  val puzzle_6_1: Array[Array[Byte]] = Array(
    Array(5, 2, 1, 1, 3, 2),
    Array(6, 1, 1, 5, 2, 2),
    Array(1, 4, 2, 3, 6, 5),
    Array(4, 1, 4, 2, 6, 6),
    Array(3, 6, 4, 6, 2, 3),
    Array(2, 6, 2, 4, 1, 3)
  ).map(_.map(_.toByte))

  val puzzle_6_2: Array[Array[Byte]] = Array(
    Array(3, 5, 2, 6, 1, 2),
    Array(5, 3, 1, 5, 3, 6),
    Array(5, 2, 4, 3, 3, 5),
    Array(4, 2, 3, 1, 5, 2),
    Array(6, 3, 3, 4, 4, 1),
    Array(6, 1, 6, 5, 4, 5)
  ).map(_.map(_.toByte))

  val puzzle_7 = Array(
    Array(4,1,6,4,5,5,2),
    Array(4,2,1,5,1,6,7),
    Array(1,5,2,1,7,2,4),
    Array(5,7,7,6,1,2,1),
    Array(1,7,4,2,2,5,3),
    Array(7,5,5,2,6,1,3),
    Array(2,3,4,4,6,3,5)
  ).map(_.map(_.toByte))

  /*
  Program body
 */
  val matrix = new Matrix(puzzle_7)
  val solver = new Solver(matrix)
  val coincidenceMap = solver.buildCoincidenceMap
  val variants: List[BitBoardRepresentation] = solver.generateVariantsForAllValues(coincidenceMap)
  println(variants.size)

  variants.map(_.toPointList)
    .filter(solver.checkSimpleConnectivity)
    .foreach(solver.outputPoint)

}

