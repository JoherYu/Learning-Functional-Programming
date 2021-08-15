package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def tail[A](as: List[A]): List[A] = as match{
        case Nil => Nil
        case Cons(x,xs) => xs        
    }

    def setHead[A](as: List[A], a: A): List[A] = Cons(a, tail(as))
    

    def drop[A](l: List[A], n: Int): List[A] = l match{
        case Nil => Nil
        case Cons(x,xs) => {
            if(n > 0) drop(xs, n-1)
            else l
        }
        
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match{
        case Nil => Nil
        case Cons(x,xs) => {
            if(f(x)) dropWhile(xs, f)
            else l
        }
    }

    def init[A](l: List[A]): List[A] = l match{
        case Nil => Nil
        case Cons (x,xs) => {
            if(xs == Nil) Nil  
            else Cons(x, init(xs))    
        }
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => y+1)
    
    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }

    def sum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
    def product(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _) 
    def lengthl[A](as: List[A]): Int = foldLeft(as, 0)((x, y) => x+1)
    def reverse[A](as: List[A]) = foldLeft(as, List[A]())((x, y) => Cons(y, x))
    def append[A](as: List[A], x: A) = foldRight(as, Cons(x, Nil: List[A]))((x, y) => Cons(x, y))
    def combine[A](as1: List[A], as2: List[A], as3: List[A]) = foldRight(as1, foldRight(as2, as3)(Cons(_, _)))(Cons(_, _))
    def plus_1(as: List[Int]): List[Int] = foldRight(as, Nil: List[Int])((x, y) => Cons(x + 1, y))
    def mkString[A](as: List[A]): List[String] = foldRight(as, Nil: List[String])((x, y) => Cons(x.toString(), y))
}



