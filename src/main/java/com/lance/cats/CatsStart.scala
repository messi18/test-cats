package com.lance.cats

import com.lance.cats.Monoid.syntax._
object CatsStart {
    def main(args:Array[String]) = {
      println("hello world")
      println("sum:", sum(List(1,2,3,4)))
      println("sum str:", sum(List("a","c","b")))
      println(s"1|+|2 = ${1|+|2}")
      println(s"""a|+|b=${"a"|+|"b"}""")
    }

    //def sum[A: Monoid](xs: List[A]): A = {
   //   val m = implicitly[Monoid[A]]
   //   xs.foldLeft(m.mzero)(m.mappend)
   // }
    def sum[M[_]: FoldLeft, A:Monoid](xs:M[A]):A = {
      val m = implicitly[Monoid[A]]
      val f1 = implicitly[FoldLeft[M]]
      f1.foldLeft(xs,m.mzero,m.mappend)
    }
}
