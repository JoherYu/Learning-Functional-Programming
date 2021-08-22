import fpinscala.datastructures.List

object solution3_List{

    // as match{
    //     case Nil => Nil
    //     case Cons(x,xs) => Cons(f(x), map(xs)(f))
    // }

    private def msgGenerate(msg: String, addtion_msg: String, list_o: String, list_n: String): String = {
        var msg_ = "%s of List %s %s: %s."
        msg_.format(msg, addtion_msg, list_o, list_n)
    }

    def main(args: Array[String]): Unit = {
        var a:List[Double] = List[Double](1, 2, 3)
        var e:List[Double] = List[Double](4, 5)
        var f:List[Double] = List[Double](6, 7, 8)
        var g:List[Double] = List[Double](2, 3)
        var h:List[Double] = List[Double](2)
        var i:List[Double] = List[Double](7)
        var j:List[Double] = List[Double](2, 7)
        var k:List[Double] = List[Double](1, 2, 3)
        var l:List[Double] = List[Double](1, 2)
        var m:List[Double] = List[Double](1, 2, 7)
        var n:List[Double] = List[Double](7, 2, 3)
        var c:List[Int] = List[Int](1, 2, 3, 4)
        println("List a call mkString: " + List.mkString(a))
        var b:List[Double] = List[Double]()
        var d:List[Int] = List[Int]()
        println(msgGenerate("get tail", "a", List.mkString(a).toString, List.mkString(List.tail(a)).toString))
        println(msgGenerate("get tail", "b", List.mkString(b).toString, List.mkString(List.tail(b)).toString))
        println(msgGenerate("set head", "a with 2.0", List.mkString(a).toString, List.mkString(List.setHead(a, 2)).toString))
        println(msgGenerate("set head", "b with 2.0", List.mkString(b).toString, List.mkString(List.setHead(b, 2)).toString))
        println("List a call mkString again : " + List.mkString(a))
        println(msgGenerate("drop 2 elements", "a", List.mkString(a).toString, List.mkString(List.drop(a, 2)).toString))
        println(msgGenerate("drop 2 elements", "b", List.mkString(b).toString, List.mkString(List.drop(b, 2)).toString))
        println(msgGenerate("drop last element", "a", List.mkString(a).toString, List.mkString(List.init(a)).toString))
        println(msgGenerate("drop last element", "b", List.mkString(b).toString, List.mkString(List.init(b)).toString))
        println(msgGenerate("length", "a", List.mkString(a).toString, List.length(a).toString))
        println(msgGenerate("length", "b", List.mkString(b).toString, List.length(b).toString))
        println(msgGenerate("leftfold length", "a", List.mkString(a).toString, List.lengthl(a).toString))
        println(msgGenerate("leftfold length", "b", List.mkString(b).toString, List.lengthl(b).toString))
        println(msgGenerate("sum", "c", List.mkString(c).toString, List.sum(c).toString))
        println(msgGenerate("sum", "d", List.mkString(d).toString, List.sum(d).toString))
        println(msgGenerate("product", "a", List.mkString(a).toString, List.product(a).toString))
        println(msgGenerate("product", "b", List.mkString(b).toString, List.product(b).toString))
        println(msgGenerate("reverse", "a", List.mkString(a).toString, List.mkString(List.reverse(a)).toString))
        println(msgGenerate("reverse", "b", List.mkString(b).toString, List.mkString(List.reverse(b)).toString))
        println(msgGenerate("append 5", "a", List.mkString(a).toString, List.mkString(List.append(a, 5)).toString))
        println(msgGenerate("append 5", "b", List.mkString(b).toString, List.mkString(List.append(b, 5)).toString))
        println(msgGenerate("combination", "a, e ,f", List.mkString(a).toString + " : " + List.mkString(e).toString + " : " + List.mkString(f).toString, List.mkString(List.combine(a, e, f)).toString))
        println(msgGenerate("combination", "a, b ,f", List.mkString(a).toString + " : " + List.mkString(b).toString + " : " + List.mkString(f).toString, List.mkString(List.combine(a, b, f)).toString))
        println(msgGenerate("plus 1", "c", List.mkString(c).toString, List.mkString(List.plus_1(c)).toString))
        println(msgGenerate("plus 1", "d", List.mkString(d).toString, List.mkString(List.plus_1(d)).toString))
        println(msgGenerate("map plus 1", "a", List.mkString(a).toString, List.map(a)((x) => (x + 1).toString).toString))
        println(msgGenerate("map plus 1", "b", List.mkString(b).toString, List.map(b)((x) => (x + 1).toString).toString))
        println(msgGenerate("filter out odd numbers", "c", List.mkString(c).toString, List.mkString(List.filter(c)(x => x % 2 == 0)).toString))
        println(msgGenerate("filter out odd numbers", "d", List.mkString(d).toString, List.mkString(List.filter(d)(x => x % 2 == 0)).toString))
        println(msgGenerate("repeat element", "a and flat it", List.mkString(a).toString, List.mkString(List.flatMap(a)(i => List(i, i))).toString))
        println(msgGenerate("repeat element", "b and flat it", List.mkString(b).toString, List.mkString(List.flatMap(b)(i => List(i, i))).toString))
        println(msgGenerate("filter out odd numbers", "c via function flatfilter", List.mkString(c).toString, List.mkString(List.flatfilter(c)(x => x % 2 == 0)).toString))
        println(msgGenerate("filter out odd numbers", "d via function flatfilter", List.mkString(d).toString, List.mkString(List.flatfilter(d)(x => x % 2 == 0)).toString))
        println(msgGenerate("add 2 |xx", "xx| Lists a and e", List.mkString(a).toString + " : " + List.mkString(e), List.mkString(List.add(a, e)).toString))
        println(msgGenerate("add 2 |xx", "xx| Lists e and a", List.mkString(e).toString + " : " + List.mkString(a), List.mkString(List.add(e, a)).toString))
        println(msgGenerate("add 2 |xx", "xx| Lists e and b", List.mkString(e).toString + " : " + List.mkString(b), List.mkString(List.add(e, b)).toString))
        println(msgGenerate("zip 2 |xx", "xx| Lists a and e with function _*_", List.mkString(a).toString + " : " + List.mkString(e), List.mkString(List.zipWith(a, e)(_ * _)).toString))
        println(msgGenerate("zip 2 |xx", "xx| Lists e and a with function _*_", List.mkString(e).toString + " : " + List.mkString(a), List.mkString(List.zipWith(e, a)(_ * _)).toString))
        println(msgGenerate("zip 2 |xx", "xx| Lists e and b with function _*_", List.mkString(e).toString + " : " + List.mkString(b), List.mkString(List.zipWith(e, b)(_ * _)).toString))       
        println(msgGenerate("sequence", "a has list g ?", List.mkString(a).toString + " : " + List.mkString(g), List.hasSubsequence(a, g).toString))
        println(msgGenerate("sequence", "g has list a ?", List.mkString(g).toString + " : " + List.mkString(a), List.hasSubsequence(g, a).toString))
        println(msgGenerate("sequence", "g has list b ?", List.mkString(g).toString + " : " + List.mkString(b), List.hasSubsequence(g, b).toString))
        println(msgGenerate("sequence", "a has list h ?", List.mkString(a).toString + " : " + List.mkString(h), List.hasSubsequence(a, h).toString))    
        println(msgGenerate("sequence", "a has list i ?", List.mkString(a).toString + " : " + List.mkString(i), List.hasSubsequence(a, i).toString))    
        println(msgGenerate("sequence", "a has list j ?", List.mkString(a).toString + " : " + List.mkString(j), List.hasSubsequence(a, j).toString))    
        println(msgGenerate("sequence", "k has list a ?", List.mkString(k).toString + " : " + List.mkString(a), List.hasSubsequence(k, a).toString))    
        println(msgGenerate("sequence", "l has list a ?", List.mkString(l).toString + " : " + List.mkString(a), List.hasSubsequence(l, a).toString))
        println(msgGenerate("sequence", "a has list l ?", List.mkString(a).toString + " : " + List.mkString(l), List.hasSubsequence(a, l).toString))
        println(msgGenerate("sequence", "a has list m ?", List.mkString(a).toString + " : " + List.mkString(m), List.hasSubsequence(a, m).toString))
        println(msgGenerate("sequence", "a has list n ?", List.mkString(a).toString + " : " + List.mkString(n), List.hasSubsequence(a, n).toString))
   
   }
}