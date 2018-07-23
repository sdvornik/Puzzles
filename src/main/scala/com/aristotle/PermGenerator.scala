package com.aristotle

class PermGenerator (private val length: Int) {

  private def initStateStorage(): Unit = {
    // allocate memory for state storage
    val bitSize = length*(length - 1)/2 - 1
    val byteSize = bitSize / 8 + Math.signum(bitSize % 8)
    println("Byte: " + byteSize)

    val intSize = bitSize / 32 + Math.signum(bitSize % 32).toInt

    println("Int: " + intSize)
  }
  initStateStorage()

}

object Test extends App {
  new PermGenerator(12)
}
