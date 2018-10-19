/**
 * 
 */

class Poly[ComplexT <: Complex[ComplexT]](val coeff: Seq[ComplexT]) {
  require(coeff.length > 0)

  val zero = coeff(0).zero
  val one = coeff(0).one
  val i = coeff(0).i

  def apply(x: ComplexT): ComplexT = {
    var accum: ComplexT = zero
    var pow: ComplexT = one
    coeff.reverse.foreach { c =>
      accum = accum + pow*c
      pow = pow*x
    }
    accum
  }

  def degree = coeff.length - 1

  def derivative: Poly[ComplexT] = {
    var d = one
    var dcoeff = coeff.reverse.tail.map { c =>
      val dc = d*c
      d += one
      dc
    }.reverse
    new Poly(if(dcoeff.length == 0) Seq(zero) else dcoeff)
  }

  def isMonic = coeff(0) == one

  def monic: Poly[ComplexT] =
      if(isMonic) this else new Poly(coeff.map { v => v/coeff(0) })

  def roots: Seq[ComplexT] = {
      if(degree <= 1) {
          return Seq.empty
      }

      if(coeff(0) != one) {
         return monic.roots
      }

      val maxIters = 1024
      val epsilon = coeff(0).dkEpsilon

      def closeEnough(r: Seq[ComplexT], r1: Seq[ComplexT]): Boolean =
          r.zip(r1).foldLeft(true) { (b, vs) =>
              b && coeff(0).dkLessThanEpsilon(vs._2 - vs._1)
          }
                                   

      val seed = coeff(0).dkSeed
      var r = (1 to degree-1).scanLeft(seed) { (v,_) => v*seed }.toSeq

      var iters = 0
      var done = false
      while(!done) {
          val r1 = r
          r = r.zipWithIndex.map { case (ri, i) =>
              val pdr = r.zipWithIndex.foldLeft(one) { (acc, rj_j) =>
                  val j = rj_j._2
                  if(i == j) {
                      acc
                  } else {
                    val rj = rj_j._1
                    acc * (ri-rj)
                  }
              }
              ri - this.apply(ri)/pdr
          }

          iters += 1

          //println(s"         iter $iters: $r1 -> $r")

          done = iters > maxIters || closeEnough(r, r1)
      }

      r
  }

  override def toString: String = {
    val sb = new StringBuilder
    var pow = coeff.length - 1
    var first = true
    coeff.foreach { c =>
      if(c != zero) {
        if(first) {
          first = false
        } else {
          sb ++= " + "
        }
        sb ++= s"($c)"
        if(pow != 0) sb ++= s".x^$pow"
      }
      pow -= 1
    }
    if(first) "0" else sb.toString
  }

}
