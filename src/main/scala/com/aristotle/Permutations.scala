package com.aristotle

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
object Permutations extends App {

  val items = Array(1,2,3,4)
  println(items.mkString(","))
  println(Calculate(Array(1,2,3,4)).mkString(","))

  def factorial(l: Int): Int = l match {
    case 0 => 1
    case 1 => 1
    case n  => n* factorial(n-1)
  }

  def Calculate(items: Array[Int]) = {
    val length = items.length
    val result = new Array[Int](factorial(length) * length)

    // val result2: Array[Array[Char]] = Array.fill(factorial(length), length)(0: Char)

    Array.copy(items, 0, result, 0, length)

    var index = 1
    (1 until length).foreach { i =>
      (1 to index).foreach { k =>
        (0 until i).foreach { _ =>
          shiftRight(result, k, i, length)
          Array.copy(result, (k-1) * length, result, index * length, length)
          index += 1
        }
        shiftRight(result, k, i, length)
      }
    }
    result
  }

  def shiftRight(items: Array[Int], i: Int, iteration: Int, length: Int): Unit = {
    val temp = items(i * length - 1)
    Array.copy(items, i * length - iteration - 1, items, i * length - iteration, iteration)
    items(i * length - iteration - 1) = temp
  }
}
