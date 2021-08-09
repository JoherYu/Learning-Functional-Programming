object solution2_1{

    private def compareChar(a: Char, b:Char): Boolean = {
        a <= b
    }

    private def compareInt(a: Int, b:Int): Boolean = {
        a <= b
    }

    private def compareIntReverse(a: Int, b:Int): Boolean = {
        a >= b
    }

    private def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
        @annotation.tailrec
        def go(n: Int) : Boolean = {
            if (n >= as.length - 1) true
            else if (ordered(as(n), as(n +1))) go(n + 1)
            else false
        }

        go(0)
    }

    private def msgGenerate(name: String, msg: String, result: Boolean): String = {
        var msg_ = "%s is a %s: %b."
        msg_.format(name, msg, result)
    }

    def main(args: Array[String]): Unit = {
        var a = Array[Int](1, 2, 3)
        var b = Array[Int](3, 2, 1)
        var c = Array[Char]('a', 'b', 'c')
        println(msgGenerate(a.mkString, "order sequence", isSorted(a, compareInt)))
        println(msgGenerate(a.mkString, "reverse order sequence", isSorted(a, compareIntReverse)))
        println(msgGenerate(b.mkString, "reverse order sequence", isSorted(b, compareIntReverse)))
        println(msgGenerate(c.mkString, "order sequence", isSorted(c, compareChar)))
    }
}