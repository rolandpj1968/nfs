/**
 * 
 */

import ComplexDouble._

class Poly[ComplexT <: Complex[ComplexT]](val coeff: Array[ComplexT]) {
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

}
