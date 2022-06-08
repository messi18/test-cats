package com.lance.cats

trait FoldLeft[F[_]] {
  def foldLeft[A,B](xs:F[A],  b: B, f:(B,A) => B):B
}
object FoldLeft {
  implicit val FoldLeftList = new FoldLeft[List] {
    override def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B): B = xs.foldLeft(b)(f)
  }
}
