package com.lance.cats

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  def size[A](tree:Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l)+ size(r)
  }
  def max(tree: Tree[Int]):Int = tree match {
    case Leaf(value) => value
    case Branch(l,r)  =>
      val lm: Int = max(l)
      val rm: Int = max(r)
      if (lm > rm)  lm else rm
  }
  def map[A,B](tree: Tree[A])(f:A => B):Tree[B] = tree match {
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
    case Leaf(value) => Leaf(f(value))
  }

//  def foldLeft[A,B](tree: Tree[A], z:B)(f:(A,B) => B):B = tree match {
//    case Leaf(value) => f(value,z)
//    case Branch(l,r) => foldLeft(l,z)(f) + foldLeft(r,z)(f)
//  }
}

