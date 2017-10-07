package com.hitori


/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

object BitBoardRepresentation {

  private def getColumnReprForArr(rowArr: Array[Int]): Array[Int] = {
    val size = rowArr.length
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

  private def check(rowArr: Array[Int]): Boolean = {
    val columnArr = getColumnReprForArr(rowArr)
    rowArr.drop(1).zip(rowArr).forall { case (a, b) => (a & b) == 0 } &&
      columnArr.drop(1).zip(columnArr).forall { case (a, b) => (a & b) == 0 }
  }

  def combine(that: BitBoardRepresentation, other: BitBoardRepresentation): Option[BitBoardRepresentation] = {
    require(that.size == other.size)
    val accArr = new Array[Int](that.size)
    that.getRowRepr.zip(other.getRowRepr).zipWithIndex.foreach { case ((selfRow, otherRow), idx) =>
      accArr(idx) = selfRow | otherRow
    }
    if(check(accArr)) Some(new BitBoardRepresentation(accArr)) else None
  }
}

final class BitBoardRepresentation(private val rowArr: Array[Int]) {
  import BitBoardRepresentation._
  private val size: Int = rowArr.length

  def this(boardSize: Int, listPoint: List[Point]) {
    this(new Array[Int](boardSize))
    listPoint.foreach(p => add(p))
  }

  def getRowRepr: Array[Int] = {
    val rowArrCopy = new Array[Int](size)
    rowArr.copyToArray(rowArrCopy)
    rowArrCopy
  }

  def getColumnRepr: Array[Int] = getColumnReprForArr(rowArr)

  @inline
  private def add(point: Point): Unit = point match {
    case Point(row, column) => rowArr(row) = rowArr(row) | (1 << (size - 1 - column))
  }

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

  def checkNeighborhood: Boolean = check(rowArr)

  def toPointList: List[Point] = rowArr.zipWithIndex.foldLeft(List.empty: List[Point]) {
    case (acc, (rowValue, rowIdx)) =>
      acc ++ (0 until size).foldLeft(List.empty: List[Point])( (acc1, columnIdx) => {
        val mask = 1 << (size - 1 - columnIdx)
        if ((mask & rowValue) != 0) Point(rowIdx.toByte, columnIdx.toByte) :: acc1
        else acc1
      })
  }

  override def toString: String = toPointList.sorted.mkString(",")
}
