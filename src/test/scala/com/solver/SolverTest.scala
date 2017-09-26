package com.solver

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

import org.scalatest._
import org.scalatest.check.Checkers

class SolverTest  extends FunSuite with Checkers {
/*
  test("Check line numbers conversion") {
    import Solver._
    val numbers1: Set[Int] = findLineNumbers(findCoincidence(Vector(0,2,2)), 3).toSet
    val controlSet1: Set[Int] = Set(2, 1)

    val numbers2: Set[Int] = findLineNumbers(findCoincidence(Vector(2,3,3,2)), 4).toSet
    val controlSet2: Set[Int] = Set(3,5,10,12)
    assert(numbers2 == controlSet2)

  }

  test("Check board check") {
    import Solver._
    val res: Boolean = checkBoard((List(5,2,5), List(5,2,5)))
    assert(res)
  }

  test("Check combine func") {
    import Solver._
    val res: (List[Int], List[Int]) = combineColumnAndRows(List(1,1,1), List(1,1,1), 3)
    println(res)
    //assert(res)
  }
  */
/*
  test("Check generate board") {
    import Solver._
    val res: List[List[Int]] = generateBoard(List(List(1,2), List(2,1), List(3,3)), null)
    println(res)
    assert(true)
  }
  */
}
