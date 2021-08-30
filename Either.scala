sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Left(x) => Left(x)
        case Right(x) => Right(f(x))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(x) => Left(x)
        case Right(x) => f(x)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(_) => b
        case Right(x) => Right(x)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
        for {
            x <- this;
            y <- b
        } yield f(x, y)
    
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
        case Nil => Right(Nil)
        case h::t => h flatMap (hh => sequence(t).map(hh::_))
    }
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
        case Nil => Right(Nil)
        case h::t => f(h) flatMap (hh => traverse(t)(f).map(hh::_))
    }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]