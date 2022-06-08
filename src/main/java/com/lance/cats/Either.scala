package com.lance.cats

sealed trait Either[+E,+A] {
  def map[B](f: A => B):Either[E,B] = this match {
    case l@Left(_) => l
    case Right(value) => Right(f(value))
  }
  def flatMap[EE>:E,B](f : A => Either[EE,B]):Either[EE,B] = this match {
    case l@Left(_) => l
    case Right(value) => f(value)
  }
  def orElse[EE>:E,B>:A](f: => Either[EE,B]):Either[EE,B] = this match {
    case Left(_) => f
    case l@Right(v) => l
  }
  def map2[EE>:E,B,C](b: Either[EE,B])(f:(A,B) => C):Either[EE,C] = this match {
    case l@Left(_) => l
    case Right(x) => b match {
      case l@Left(_) => l
      case Right(y) => Right(f(x,y))
    }
  }
}
case class Left[+E](value:E) extends Either[E, Nothing]
case class Right[+A](value:A) extends Either[Nothing,A]

object Either {
  def Try[A](a: =>A): Either[Exception,A] = try {
    Right(a)
  } catch {case e: Exception => Left(e)}
  def sequence[E,A](es: List[Either[E,A]]):Either[E,List[A]] = es match {
    case Nil => Right(Nil)
    case Cons(Right(v),xs) => sequence(xs).map(Cons(v,_))
    case Cons(l@Left(_), _) => l
  }
  def traverse[E,A,B](as:List[A])(f: A => Either[E,B]):Either[E,List[B]] = as match {
    case Nil => Right(Nil)
    case Cons(v,xs) => traverse(xs)(f).flatMap(l => f(v).map(Cons(_,l)))
  }
}



