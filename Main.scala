import ComplexDouble._

object MyMain {
     def p(c: ComplexDouble*) = new Poly(c)
     
     def c(d: Double) = d + 0*i

     val P1 = p(c(1), c(-3), c(2))
     val P2 = p(c(1), c(-2), c(1))
     val P3 = p(c(1), c(0), c(4))
     val P4 = p(c(4), c(0), c(1))
     val P5 = p(c(1), c(-3), c(2), c(0))
     val P6 = p(c(1), c(0), c(-13), c(0), c(36))
     val P7 = p(c(1), c(1), c(1))
     val P8 = p(c(1), c(1), c(1), c(1))
     val P9 = p(c(1), c(1), c(1), c(1), c(1))

     def solve(p: Poly[ComplexDouble]): Unit = {
         println(s"$p: roots at ${p.roots}")
     }

     def main(args:Array[String]):Unit = {
         println("Hello rolando")
         val c = new ComplexDouble(0.4, 0.9)
         println(s"c is $c, c^2 = ${c*c}")

         // val p = new Poly(Seq(c, c*c, c*c*c))
         // println(s"p is $p, p(1) = ${p(1)}")
         
         // val p2 = new Poly(Seq(1+0*i, 2+0*i, 1+0*i))
         // println(s"p2 is $p2, p2(1) = ${p2(1)} p2(2) = ${p2(2)} p2(3) = ${p2(3)}")
         
         // println(s"p2 is $p2, p2 roots are = ${p2.roots}")

         for(p <- Seq(P1, P2, P3, P4, P5, P6, P7, P8, P9)) {
               solve(p)
         }
     }      
}

