package com.lance.cats

object ExceptionHandler {
  def failingFn(i:Int): Int = {
    val y:Int = throw new Exception("error")
    try {
      val x = 42 + 5
      x + y
    } catch {case e : Exception => 43}
  }

  def mean(xs:Seq[Double]):Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs:Seq[Double]):Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x- m, 2))))

  def lift[A,B](f: A => B):Option[A] => Option[B] = _ map f

  def Try[A](a: => A):Option[A] = try Some(a) catch {case e:Exception => None}
  def parseInsuranceRateQuote(age:String, numberOfSpeedingTickets:String):Option[Double] = {
    val optAge = Try{age.toInt}
    val optTickets = Try{numberOfSpeedingTickets.toInt}
//    optAge.flatMap(age => optTickets.map(t => insuranceRateQuote(age, t)))
    map2(optAge,optTickets)(insuranceRateQuote)
  }
  def map2[A,B,C](a:Option[A], b:Option[B])(f: (A,B) => C) = a.flatMap(x => b.map(y => f(x,y)))
  def insuranceRateQuote(age:Int, tickets: Int):Double  = {
    age*1.0/tickets
  }
  def sequence[A](options:List[Option[A]]):Option[List[A]] = options match {
    case Nil => Some(Nil)
    case Cons(Some(v) ,xs)  => sequence(xs).map(Cons(v, _))
    case Cons(None ,_)  => None
//    case Cons(None ,xs)  => sequence(xs)
  }

  def traverse[A,B](a:List[A])(f: A => B):Option[List[B]] = a match {
    case Nil => Some(Nil)
    case Cons(h, xs) => Try(f(h)).flatMap(v => traverse(xs)(f).map(Cons(v,_)))
  }
  def main(args:Array[String]): Unit = {
//    val f: Option[Int] => Option[Int] = lift(math.abs)
    val list = Cons(Some(1),Cons(None,Cons(Some(3),Nil)))
    println(sequence(list))
    val list2 = Cons("1",Cons("3",Cons("4",Nil)))
    println(traverse(list2)(_.toInt))
  }
}

sealed trait Option[+A] {
  def flatMap[B](f: A => Option[B]):Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }
  def map[B](f: A => B):Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }
  def getOrElse[B>:A](default: => B):B = this match {
    case None => default
    case Some(v) => v
  }
  def orElse[B>:A](ob: => Option[B]):Option[B] = this match {
    case None => ob
    case s@Some(_) => s
  }
  def filter(f: A => Boolean):Option[A] = this match {
    case None => None
    case Some(v) => if (f(v)) Some(v) else None
  }
}
case class  Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]

