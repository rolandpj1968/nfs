/**
 * 
 */

import ComplexDouble._

class Poly[ComplexT <: Complex[ComplexT]](val coeff: Seq[ComplexT]) {
  require(coeff.length > 0)

  val zero = coeff(0).zero
  val one = coeff(0).one

  def apply(x: ComplexT): ComplexT = {
    var accum: ComplexT = zero
    var pow: ComplexT = one
    coeff.reverse.foreach { c =>
      accum = accum + pow*c
      pow = pow*x
    }
    accum
  }

  def derivative: Poly[ComplexT] = {
    var d = one
    var dcoeff = coeff.reverse.tail.map { c =>
      val dc = d*c
      d += one
      dc
    }.reverse
    new Poly(if(dcoeff.length == 0) Seq(zero) else dcoeff)
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
