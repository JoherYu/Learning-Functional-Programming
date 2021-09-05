import Stream._
trait Stream[+A] {
    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(x,xs) => x() :: xs().toList
    }

    def drop(n: Int): Stream[A] = this match{
        case Empty => this
        case Cons(x,xs) => if(n == 0) this else xs().drop(n-1)
    }

    def take(n: Int): Stream[A] = this match {
        case Empty => this
        case Cons(x,xs) => if(n == 0) Empty else Cons(x, () => xs().take(n-1))
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Empty => this
        case Cons(x,xs) => if(p(x())) Cons(x, () => xs().takeWhile(p)) else Empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
        this match {
            case Cons(h, t) => f(h(), t().foldRight(z)(f))
            case _ => z
        }

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
    def takeWhile_f(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else empty)
    def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))
    def map[B](f: A => B): Stream[B] = foldRight(empty: Stream[B])((x, y) => cons(f(x), y))
    def filter(f: A => Boolean): Stream[A] = 
        foldRight(empty: Stream[A])((x, y) => {
            if(f(x)) cons(x, y)
            else y
        })
    def append[B>:A](x: => Stream[B]): Stream[B] = foldRight(x)((x, y) => cons(x, y))
    def flatMap[B](f: A => Stream[B]): Stream[B] = {
        foldRight(empty: Stream[B])((x, y) => f(x) append y)
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
    def fib: Stream[Int] = {
        def go(a: Int,b: Int): Stream[Int] = {
            cons(a, go(b, a+b))
        }
        go(0, 1)
    }
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
        case None => empty
        case Some((a, b)) => cons(a, unfold(b)(f))
    }
}