import fpinscala.datastructures.List

object solution3_List{

    private def msgGenerate(msg: String, addtion_msg: String, list_o: String, list_n: String): String = {
        var msg_ = "%s of List %s %s: %s."
        msg_.format(msg, addtion_msg, list_o, list_n)
    }

    def main(args: Array[String]): Unit = {
        var a:List[Double] = List[Double](1, 2, 3)
        var e:List[Double] = List[Double](4, 5)
        var f:List[Double] = List[Double](6, 7, 8)
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
    }
}