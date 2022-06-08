package com.lance.cats

class ParallelObject {

}
object ParallelObject {
  def  partial1[A,B,C](a:A, f:(A,B)=>C):B => C = (b:B) => f(a,b)
  def main(args:Array[String]): Unit = {
    println("Hello")
  }
}
