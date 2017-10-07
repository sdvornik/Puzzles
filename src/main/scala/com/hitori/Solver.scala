package com.hitori


/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

class Matrix(private val matrix: Array[Array[Byte]]) {
  require(matrix.nonEmpty && matrix.forall(l => l.length == matrix.length))

  val size: Int = matrix.length

  def apply(rowIdx: Int, columnIdx: Int): Byte = matrix(rowIdx)(columnIdx)
}

sealed trait SuperPoint
case object Pole extends SuperPoint
case class Point(row: Byte, column: Byte) extends Ordered[Point] with SuperPoint {

  import scala.math.Ordered.orderingToOrdered

  def compare(that: Point): Int = (this.row, this.column) compare(that.row, that.column)
}

final class UndirectedEdge(val point1: SuperPoint, val point2: SuperPoint) {
  override def hashCode(): Int = point1.hashCode() | point2.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: UndirectedEdge => (that.point1==this.point1 && this.point2 == that.point2) ||
      (that.point1==this.point2 && this.point2 == that.point1)
    case _ => false
  }

  override def toString: String = s"UndirectedEdge(${this.point1}, ${this.point2})"

  def getOtherVertex(point: SuperPoint): SuperPoint = {
    if(point == point1) point2
    else if(point == point2) point1
    else throw new IllegalArgumentException("Edge not contain this point")
  }
}

class Solver(private val matrix: Matrix) {

  /*
  Find coincide number in row or column.
   */
  private val size = matrix.size

  def buildCoincidenceMap: Map[Byte,(Map[Byte, List[Byte]], Map[Byte, List[Byte]])] =
    (for (i <- 0 until size; j <- 0 until size) yield (matrix(i, j), (i, j)))
    /*
    Group by value
     */
  .foldLeft(Map.empty[Byte, List[(Byte, Byte)]]) {
      case (acc, (value, (row, column))) =>
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
      case (value, (rowMap, columnMap)) =>
        (
          value,
          (
            rowMap.filter { case (_, columnIdxList) => columnIdxList.length > 1 },
            columnMap.filter { case (_, rowIdxList) => rowIdxList.length > 1 }
          )
        )
    }
    .filter { case (_, (rowMap, columnMap)) => rowMap.nonEmpty || columnMap.nonEmpty }


  def toPointSet(coincidenceTuple: (Map[Byte, List[Byte]], Map[Byte, List[Byte]])): Set[Point] = {
    val (rowMap, columnMap) = coincidenceTuple
    (rowMap.flatMap { case (row, columnList) => columnList.map(Point(row, _)) } ++
    columnMap.flatMap { case (column, rowList) => rowList.map(Point(_, column)) }).toSet
  }

  def generateVariantsForWhitePoints(numberOfWhitePoints: Int, pointSet: Set[Point]): Iterator[Set[Point]] = pointSet
    .toList
    .combinations(numberOfWhitePoints)
    .filter(list => new BitBoardRepresentation(size, list).checkCharge)
    .map(list => pointSet -- list)


  def generateAllVariantsForCertainValue(pointSet: Set[Point]): List[BitBoardRepresentation] = {

    def genRec(number: Int, acc: List[BitBoardRepresentation]): List[BitBoardRepresentation] = {
      generateVariantsForWhitePoints(number, pointSet)
        .toList.map( set => new BitBoardRepresentation(size, set.toList) ) match {
          case Nil => acc
          case nonEmptyList =>
            genRec(number + 1, nonEmptyList) ++ acc
      }
    }
    genRec(0, List.empty).filter(_.checkNeighborhood)
  }

  def generateVariantsForAllValues(coincidenceMap: Map[Byte,(Map[Byte, List[Byte]], Map[Byte, List[Byte]])]): List[BitBoardRepresentation] = {
    val list: List[List[BitBoardRepresentation]] = coincidenceMap.map {
      case(_, coincidenceTuple) => generateAllVariantsForCertainValue(toPointSet(coincidenceTuple))
    }.toList
    list.tail.foldLeft(list.head) ((acc, nextList) => acc
      .flatMap( prevBoard =>
        nextList.map( nextBoard =>
          BitBoardRepresentation.combine(prevBoard, nextBoard)
        )
      )
      .foldLeft(List.empty: List[BitBoardRepresentation]) {
        case (resAcc, None) => resAcc
        case (resAcc, Some(elm)) => elm :: resAcc
      }
    )
  }

  def buildEdges(coincidenceMap: Map[Byte,(Map[Byte, List[Byte]], Map[Byte, List[Byte]])]): Unit = coincidenceMap.map {
    case (value, (rowMap, columnMap)) =>
      rowMap.map {
        case (row, columnList) =>
          val points = columnList.map(Point(row,_))
          null
          // points.drop(1).zip(points).map
      }
  }
}
