package com.lance.cats

object List {
  def sum(ints:List[Int]):Int = foldRight(ints,0)(_+_)
//    ints match {
//    case Nil => 0
//    case Cons(x,xs) => x + sum(xs)
//  }
  def product(ds:List[Double]):Double = foldRight(ds,1.0)(_*_)
//  ds match {
//    case Nil => 1.0;
//    case Cons(0,_) => 0.0
//    case Cons(x,xs) => x * product(xs)
//  }

  //  https://stackoverflow.com/questions/7938585/what-does-param-mean-in-scala
  def apply[A](as:A*):List[A] = if (as.isEmpty) Nil else Cons(as.head,apply(as.tail: _*)) // todo _*   是啥意思
  def tail[A](ls:List[A]):List[A] = ls match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }
  def setHead[A](h:A, ls:List[A]):List[A] = ls match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }
  def drop[A](l:List[A], n:Int):List[A] = l match {
    case Nil => Nil
    case Cons(h,xs) =>
      n match {
        case 0 => xs
        case _ => Cons(h, drop(xs, n-1))
      }
  }
  def dropWhile[A](l:List[A])(f: A => Boolean):List[A] = l match {
    case Cons(h,xs) if f(h) => dropWhile(xs)(f)
    case Cons(head, tail) => Cons(head, dropWhile(tail)(f))
    case Nil => Nil
  }

  def append[A](a1:List[A], a2:List[A]):List[A] = foldLeft(a1,a2)(Cons(_,_))
//    a1 match {
//    case Nil => a2
//    case Cons(h, xs) => Cons(h,append(xs, a2))
//  }

  def init[A](ls:List[A]):List[A] = ls match {
    case Nil => Nil
    case Cons(a,Cons(_, Nil)) => Cons(a,Nil)
    case Cons(a,Nil) => Cons(a, Nil)
    case Cons(a,xs) => Cons(a, init(xs))
  }

  def foldRight[A,B](list: List[A], z:B)(f: (A,B) => B): B = list match {
    case Nil => z
//    case Cons(h,xs) => f(h,foldRight(xs,z)(f))
    case Cons(h,xs) => foldLeft(xs,f(h,z))(f)
  }

  def length[A](list: List[A]) = foldRight(list,0)((_, b) => 1+b)

  def foldLeft[A,B](list: List[A], z:B)(f:(A,B) => B):B = list match {
    case Cons(head, tail) => foldLeft(tail,f(head,z))(f)
    case Nil => z
  }

  def filter[A](list: List[A])(f: A => Boolean):List[A] =
//    list match {
//    case Cons(h,g) if f(h) => filter(g)(f)
//    case Cons(h,g) =>  Cons(h,filter(g)(f))
//    case Nil => Nil
//  }
  flatMap(list)(a => if (f(a)) Nil else Cons(a,Nil))

  def reverse[A](list: List[A]):List[A] = {
//    var res : List[A] = Nil
//    def i_rev(init:List[A]):Unit = init match {
//      case Nil =>
//      case Cons(h,g)  =>
//        res = Cons(h,res)
//        i_rev(g)
//    }
//    i_rev(list)
//    res
    var result : List[A] = Nil
    var init:List[A] = list
    def iter_v(): Unit =  {
      init match {
        case Nil =>
        case Cons(h,t) =>
          result = Cons(h,result)
          init = t
          iter_v()
      }
    }
    iter_v()
    result
  }

  def map[A, B](list: List[A])(f: A => B): List[B] =
    list match {
      case Nil => Nil
      case Cons(h, g) => Cons(f(h), map(g)(f))
    }

  def flatMap[A,B](list: List[A])(f: A => List[B]):List[B] = list match {
    case Cons(head, tail) =>
      val sl = f(head)
      val nl  = flatMap(tail)(f)
      append(sl,nl)

    case Nil => Nil
  }

  def zipWith[A,B](list1:List[A], list2: List[A])(f:(A,A) => B):List[B] =
      (list1,list2) match {
        case (Cons(h1,g1),Cons(h2,g2)) => Cons(f(h1,h2), zipWith(g1,g2)(f))
        case _ => Nil

      }


  def main(args:Array[String]) = {
    val list = List(1,2,3,4)
    val list2 = List(3,6,9,12,15)
    val r = dropWhile(list)( x => x > 2)
    // foldRight(List(1,2,3,4), Nil:List[Int])(Cons(_,_)) copy list
    println(s"length:", length(list))
    println(s"fold left length:", foldLeft(list,0)((_,b) => b + 1))
    println(s"reverse:", reverse(list))
    println(s"map:", map(list)(_+1))
    println(s"map:", flatMap(list)(a=>List(a,a)))
    println(s"filter:", filter(list)( _  % 2 == 1))
    println(s"zip:", zipWith(list, list2)( _+_))
    println(r)
  }
}
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]
