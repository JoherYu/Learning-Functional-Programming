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

    
}