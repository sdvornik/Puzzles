package com.aristotle


object Solver extends App {

  /*
  Constraint matrix
   */
  val variableMatrix = Array(
    Vector(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0),
    Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1),

    Vector(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector(0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
    Vector(0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0),
    Vector(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0),
    Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1),
    Vector(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    Vector(0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0),
    Vector(1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1),
    Vector(0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0),
    Vector(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0)
  )

  val variablesQuantity = variableMatrix(0).length
  val equationQuantity = variableMatrix.length

  /*
  Constraint matrix in Rational terms
   */
  val rationalMatrix = variableMatrix.
    map(vector => (vector :+ 38).map(elm => Rational(elm)))

  /*
  Helper method for Gaussian elimination https://en.wikipedia.org/wiki/Gaussian_elimination
   */
  def swapColumns(i: Int, j: Int): Unit = {
    val temp = (rationalMatrix(i), rationalMatrix(j)).swap
    rationalMatrix.update(i, temp._1)
    rationalMatrix.update(j, temp._2)
  }
  /*
  Helper method for Gaussian elimination
   */
  def multiplyColumn(i: Int, c: Rational): Unit = rationalMatrix.update(i, rationalMatrix(i).map(_ * c))
  /*
  Helper method for Gaussian elimination
   */
  def mixColumns(i: Int, j: Int, c: Rational): Unit = rationalMatrix
    .update(i, rationalMatrix(i).zip(rationalMatrix(j)).map { case (a, b) => a + (c * b) })

  // Implementation of Gaussian elimination
  var equation = 0
  var variable = 0
  while (equation < equationQuantity && variable < variablesQuantity) {
    rationalMatrix.zipWithIndex
      .drop(equation)
      .find(v => v._1(variable).compare(Rational(0)) != 0) match {
      case Some((_, j)) =>
        swapColumns(equation, j)
        multiplyColumn(equation, Rational(1) / rationalMatrix(equation)(variable))
        rationalMatrix.zipWithIndex
          .foreach {
            case (vector, index) if index != equation => mixColumns(index, equation, -vector(variable))
            case _ =>
          }
        equation += 1
      case _ =>
    }
    variable += 1
  }

  /*
  Output matrix after gaussian elimination and removing empty rows.
  Convert type of elements to Int for better perfomance.
   */
  val outputMatrix = rationalMatrix
    .map(_.map(r => r.strictToInt))
    .filter(!_.forall(_ == 0))

  /*
  Matrix rang
   */
  val rang = outputMatrix.length

  /*
  Get diagonal elements indexes
   */
  val diagElmIndexes: Array[(Int, List[(Int, Int)])] = outputMatrix.map(vector =>
    vector.zipWithIndex.toList
      .filter {
        case (elm, index) => elm != 0 && index < variablesQuantity
      } match {
      case head :: tail => (head._2, tail)
      case _ => throw new Exception("Can't find diag element")
    }
  )

  /*
  Get independent element indexes
   */
  val freeElmIndexes: Array[Int] = (outputMatrix.flatMap(vector =>
    vector.zipWithIndex
      .filter {
        case (elm, index) => elm != 0 && index < variablesQuantity
      }
      .map(x => x._2)
  ).toSet -- diagElmIndexes.map(_._1)).toArray.sorted

  /*
  Create matrix for connecting all variables with independent elements
   */
  val newMatrix = new Array[Array[Int]](variablesQuantity)
  freeElmIndexes.zipWithIndex
    .foreach {
      case (elm, index) =>
        val arr = new Array[Int](freeElmIndexes.length)
        arr(index) = 1
        newMatrix.update(elm, arr)
    }

  val freeElmIndexiesMap = freeElmIndexes.zipWithIndex.toMap

  diagElmIndexes.foreach {
    case (index, tupleList) =>
      val arr = new Array[Int](freeElmIndexes.length)
      tupleList.foreach {
        case (coeff, idx) =>
          arr.update(freeElmIndexiesMap(idx), -coeff)
      }
      newMatrix.update(index, arr)
  }

  /*
  Create vector for connecting all variables with independent elements
   */
  val extendedVector = new Array[Int](variablesQuantity)
  val diagIndexesMap = diagElmIndexes.map {
    case (index, _) => index
  }.zipWithIndex.toMap

  diagElmIndexes.foreach {
    case (index, _) => extendedVector.update(index, outputMatrix(diagIndexesMap(index)).last)
  }

  /*
  Calculate variable of index i with independent permutation
   */
  def calculateValue(i: Int, permutation: Seq[Int]): Int = {
    extendedVector(i) + permutation.zip(newMatrix(i)).foldLeft(0)((acc, a) => acc + a._1 * a._2)
  }

  /*
  Output result in readable view
   */
  def outputResult(permutation: Seq[Int]): Unit = {
    val resultArr: Seq[Int] = (0 until variablesQuantity).map(idx => calculateValue(idx, permutation))

    println()
    print("%8s".format(""))
    (0 until 3).foreach(idx =>
      print("%2d ".format(resultArr(idx)))
    )
    println()
    print("%6s".format(""))
    (3 until 7).foreach(idx =>
      print("%2d ".format(resultArr(idx)))
    )
    println()
    print("%4s".format(""))
    (7 until 12).foreach(idx =>
      print("%2d ".format(resultArr(idx)))
    )
    println()
    print("%6s".format(""))
    (12 until 16).foreach(idx =>
      print("%2d ".format(resultArr(idx)))
    )
    println()
    print("%8s".format(""))
    (16 until 19).foreach(idx =>
      print("%2d ".format(resultArr(idx)))
    )
    println()
  }

  def permutationStream[T](xs: List[T]): Stream[List[T]] = xs match {
    case Nil => Stream.empty
    case single @ e :: Nil => Stream(single)
    case list =>
      list.indices.toStream
        .map(
          i => list.splitAt(i) match {
            case (first: List[T], second: List[T]) => (second.head, first ::: second.tail)
          }
        )
        .flatMap {
          case (elm, remainingList) => permutationStream(remainingList).map(ys => elm :: ys)
        }
  }





  /*
  All possible combinations of size variablesQuantity - rang in range 0 until 19
   */
  val combinations: Iterator[Seq[Int]] = (1 to variablesQuantity).combinations(variablesQuantity - rang)

  while (combinations.hasNext) {
    // All possible permutation in given combination
    val pStream: Stream[List[Int]] = permutationStream(combinations.next().toList)
    pStream.foreach( permutation => {
      val res = (0 until variablesQuantity).foldLeft((true, Map.empty[Int, Int])) {
        case ((true, map), idx) =>
          val value: Int = calculateValue(idx, permutation)
          if (0 < value && value < 20 && !map.contains(value)) (true, map + (value -> idx))
          else (false, Map.empty[Int, Int])
        case _ => (false, Map.empty[Int, Int])
      } match {
        case (true, map) =>
          // additional checking (not necessary)
          val swapMap = map.map { case (value, idx) => (idx, value) }
          variableMatrix.foldLeft(true) {
            case (true, row) =>
              row.zipWithIndex.foldLeft(0) {
                case (acc, (elm, idx)) => acc + swapMap(idx)*elm
              } == 38
            case _ => false
          }
        case _ => false
      }
      if (res) {
        println(permutation +";  "+permutation.sum)
        outputResult(permutation)
      }
    })
/*
    while (permutations.hasNext) {
      val permutation: Seq[Int] = permutations.next()

      val res = (0 until variablesQuantity).foldLeft((true, Map.empty[Int, Int])) {
        case ((true, map), idx) =>
          val value: Int = calculateValue(idx, permutation)
          if (0 < value && value < 20 && !map.contains(value)) (true, map + (value -> idx))
          else (false, Map.empty[Int, Int])
        case _ => (false, Map.empty[Int, Int])
      } match {
        case (true, map) =>
          // additional checking (not necessary)
          val swapMap = map.map { case (value, idx) => (idx, value) }
          variableMatrix.foldLeft(true) {
            case (true, row) =>
              row.zipWithIndex.foldLeft(0) {
                case (acc, (elm, idx)) => acc + swapMap(idx)*elm
              } == 38
            case _ => false
          }
        case _ => false
      }
      if (res) {
        println(permutation +";  "+permutation.sum)
        outputResult(permutation)
      }
    }
    */
  }

}
