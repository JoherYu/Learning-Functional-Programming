trait RNG {
    def nextInt: (Int, RNG)
}

object RNG {
    case class SimpleRNG(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            var newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
            var nextRNG = SimpleRNG(newSeed)
            var n = (newSeed >>> 16).toInt

            (n, nextRNG)
        }

        def nonNegativeInt(rng: RNG): (Int, RNG) = {
            var (n, nextRNG) = rng.nextInt
            var nextInt = if (n < 0) -(n + 1) else n
            (nextInt, nextRNG)
        }

        def double(rng: RNG): (Double, RNG) = {
            var (n, nextRNG) = nonNegativeInt(rng)
            var nextDouble = n / (Int.MaxValue.toDouble + 1)
            (nextDouble, nextRNG)
        }
        
        def intDouble(rng: RNG): ((Int ,Double), RNG) = {
            var (i, nextRNG) = rng.nextInt
            var (d, nextRNG1) = double(rng)
            ((i, d), nextRNG)
        }        
        
        def doubleInt(rng: RNG): ((Double, Int), RNG) = {
            var ((i, d), nextRNG) = intDouble(rng)
            ((d, i), nextRNG)
        }        
        
        def double3(rng: RNG): ((Double, Double, Double), RNG) = {
            var (d1, rng1) = double(rng)
            var (d2, rng2) = double(rng1)
            var (d3, rng3) = double(rng2)
            ((d1, d1, d3), rng3)
        }

        def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
            if(count == 0){
               (Nil, rng) 
            }else{
                var (n, nextRNG) = rng.nextInt
                var (n1, nextRNG1) = ints(count-1)(nextRNG)
                (n::n1, nextRNG1)
            } 
        }
    type Rand[+A] = RNG => (A, RNG)
    def map[A,B](s: Rand[A])(f: A => B): Rand[B] = 
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def double_m: Rand[Double] = map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
        rng => {
            val (a1, rng1) = ra(rng)
            val (a2, rng2) = rb(rng1)
            (f(a1, a2), rng2)
        }

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
        rng => {
            val (a1, rng1) = f(rng)
            g(a1)(rng1)
        }    

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def nonNegativeLessThan(n: Int): Rand[Int] =
        flatMap(nonNegativeInt) { i =>
            val mod = i % n
            if(i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)

        }

    def map_f[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(i => unit(f(i)))
    def map2_F[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b =>f(a, b)))
    }
}