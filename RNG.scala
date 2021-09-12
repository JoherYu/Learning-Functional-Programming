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
    }
}