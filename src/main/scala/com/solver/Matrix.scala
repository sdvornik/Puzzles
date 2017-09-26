package com.solver

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

class Matrix(private val matrix: Array[Array[Byte]]) {
  require(matrix.nonEmpty && matrix.forall(l => l.length == matrix.length))

  val size: Int = matrix.length

  def get(rowIdx: Int): Vector[Byte] = matrix(rowIdx).toVector

  def get(rowIdx: Int, columnIdx: Int): Byte = matrix(rowIdx)(columnIdx)

  def transpose: Matrix = {
    val newMatrix: Array[Array[Byte]] = Array.fill(matrix.length, matrix.length)(0.toByte)
    for(i <- matrix.indices; j <- matrix.indices ) newMatrix(i)(j) = matrix(j)(i)
    new Matrix(newMatrix)
  }
}
