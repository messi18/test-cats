package com.lance.cats

trait Monoid[A] {
  def mappend(a1:A, a2:A): A
  def mzero: A
}
object Monoid {
  object syntax extends MonoidSyntax
  implicit val IntMonoid =  new Monoid[Int]{
    def mappend(a:Int, b:Int): Int = a + b
    def mzero: Int = 0
  }
  implicit val StringMonoid = new Monoid[String] {
    override def mappend(a1: String, a2: String): String = a1 + a2
    override def mzero: String = ""
  }
}

trait MonoidSyntax {
  implicit def monoidSyntax[A: Monoid](a: A): MonoidOps[A] = new MonoidOps[A](a)
}
final class MonoidOps[A:Monoid](lhs:A) {
  def |+|(rhs: A): A = implicitly[Monoid[A]].mappend(lhs, rhs)
}
