package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def size[A](as: Tree[A]): Int = as match{
        case Leaf(v) => 1
        case Branch(l, r) => size(l) + size(r) + 1
    }

    def maximum(as: Tree[Int]): Int = as match{
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    def depth[A](as: Tree[A]): Int = as match{
        case Leaf(v) => 0
        case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    def map[A,B](as: Tree[A])(f: A => B): Tree[B] = as match{
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
    
    def fold[A,B](as: Tree[A])(fl: A => B)(fb: (B, B) => B): B = as match {
        case Leaf(v) => fl(v)
        case Branch(l, r) => fb(fold(l)(fl)(fb), fold(r)(fl)(fb))
    }

    def sizeViaFold[A](as: Tree[A]): Int = fold(as)(_ => 1)((x: Int ,y: Int) => x + y + 1)
    def maximumViaFold(as: Tree[Int]): Int = fold(as)(x => x)((x: Int,y: Int) => x max y)
    def depthViaFold[A](as: Tree[A]): Int = fold(as)(_ => 0)((x: Int ,y: Int) => (x max y) + 1)
    def mapViaFold[A,B](as: Tree[A])(f: A => B): Tree[B] = fold(as)(x => Leaf(f(x)): Tree[B])((x: Tree[B] ,y: Tree[B]) => Branch(x, y))

}