package com.lance.cats

object CurryObject {
  def curr[A,B,C](f:(A,B)=>C):A => (B => C) = (a:A) => (b:B) => f(a,b)
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a, b) => f(a)(b)
  def compose[A,B,C](h:B => C, g: A => B ): A => C = (a:A) => h(g(a))
}
