package com.hitori

import java.util

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

class BitBoardRepresentation(private val size: Int) {

  private val rowArr = new Array[Int](size)

  def add(listPoint: List[Point]): this.type = {
    listPoint.foreach(p => add(p))
    this
  }

  def getRowRepr: Array[Int] = {
    val rowArrCopy = new Array[Int](size)
    rowArr.copyToArray(rowArrCopy)
    rowArrCopy
  }

  def getColumnRepr: Array[Int] = {
    val columnArr = new Array[Int](size)
    rowArr.zipWithIndex.foreach { case(row, rowIdx) =>
      (0 until size).foreach(columnIdx =>
        if(((1 << (size - 1 - columnIdx)) & row) != 0) {
          columnArr(columnIdx) = (1 << (size - 1 - rowIdx)) | columnArr(columnIdx)
        }
      )
    }
    columnArr
  }

  private def add(point: Point): Unit = point match {
    case Point(row, column) => rowArr(row) = rowArr(row) | (1 << (size - 1 - column))
  }

  def check: Boolean = {
    val columnArr = getColumnRepr
    rowArr.drop(1).zip(rowArr).forall { case (a, b) => (a & b) == 0 } &&
      columnArr.drop(1).zip(columnArr).forall { case (a, b) => (a & b) == 0 }
  }

  def clear(): Unit = util.Arrays.fill(rowArr, 0)

  private def checkChargeForRepr(arrRepr: Array[Int]): Boolean = {
    (0 until size).forall( idx => {
      val mask = 1 << (size - 1 - idx)
      val (_, isNotExceedOne) = arrRepr.foldLeft((0, true)) {
        case ((chargeBitVector, true), arrElm) =>
          if((chargeBitVector & mask) != 0 && (arrElm & mask) != 0) (0, false)
          else ((chargeBitVector & mask) | (arrElm & mask), true)
        case ((_, false), _) => (0, false)
      }
      isNotExceedOne
    })
  }

  def checkCharge: Boolean = checkChargeForRepr(rowArr) && checkChargeForRepr(getColumnRepr)

}
