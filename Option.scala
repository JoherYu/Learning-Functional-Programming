sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(x) => Some(f(x))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(x) => f(x)
    }
    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(x) => x
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case Some(x) => this
    }
    def filter(f: A => Boolean): Option[A] = this match {
        case None => None
        case Some(x) => if(f(x)) this
                        else None      
    }

    def mean(xs: Seq[Double]): Option[Double] =
        if(xs.isEmpty) None
        else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = 
        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
        a.flatMap(aa => b.map(bb => f(aa, bb)))

    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
        case Nil => Some(Nil)
        case h::t => h flatMap (hh => sequence(t).map(hh::_))
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
        case Nil => Some(Nil)
        case h::t => f(h) flatMap (hh => traverse(t)(f).map(hh::_))
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]