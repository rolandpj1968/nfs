import scala.math._

trait Complex[ComplexType] {
  type RealT
  
  def re: RealT
  def im: RealT

  def zero: ComplexType
  def one: ComplexType

  def unary_+ : ComplexType
  def unary_- : ComplexType
  def unary_~ : ComplexType // conjugate
  def unary_! : RealT // modulus

  def +(c: ComplexType): ComplexType
  def -(c: ComplexType): ComplexType
  def *(c: ComplexType): ComplexType
  def /(c: ComplexType): ComplexType
}

case class ComplexDouble(re: Double, im: Double) extends Complex[ComplexDouble] with Ordered[ComplexDouble] {
  type RealT = Double

  private val modulus = sqrt(pow(re, 2) + pow(im, 2))
  
  // Constructors
  def this(re: Double) = this(re, 0)

  def zero = ComplexDouble.ZERO
  def one = ComplexDouble.ONE
  
  // Unary operators
  def unary_+ = this
  def unary_- = new ComplexDouble(-re, -im)
  def unary_~ = new ComplexDouble(re, -im) // conjugate
  def unary_! = modulus
  
  // Comparison
  def compare(that: ComplexDouble) = !this compare !that
  
  // Arithmetic operations
  def +(c: ComplexDouble) = new ComplexDouble(re + c.re, im + c.im)
  def -(c: ComplexDouble) = this + -c
  def *(c: ComplexDouble) = 
    new ComplexDouble(re * c.re - im * c.im, im * c.re + re * c.im)
  def /(c: ComplexDouble) = {
    require(c.re != 0 || c.im != 0)
    val d = pow(c.re, 2) + pow(c.im, 2)
    new ComplexDouble((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }
  
  // String representation
  override def toString() = 
    this match {
      case ComplexDouble.i => "i"
      case ComplexDouble(re, 0) => re.toString
      case ComplexDouble(0, im) => im.toString + "*i"
      case _ => asString 
    } 
  private def asString = 
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"  
}

object ComplexDouble {
  // Constants
  val i = new ComplexDouble(0, 1)
  val ZERO = new ComplexDouble(0)
  val ONE = new ComplexDouble(1)
  
  // Factory methods
  def apply(re: Double) = new ComplexDouble(re)
  
  // Implicit conversions
  implicit def fromDouble(d: Double) = new ComplexDouble(d)
  implicit def fromFloat(f: Float) = new ComplexDouble(f)
  implicit def fromLong(l: Long) = new ComplexDouble(l)
  implicit def fromInt(i: Int) = new ComplexDouble(i)
  implicit def fromShort(s: Short) = new ComplexDouble(s)
}

import ComplexDouble._
