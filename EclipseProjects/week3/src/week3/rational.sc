package week3

object rationals {
  val x = new Rational(1, 3)                      //> x  : week3.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : week3.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : week3.Rational = 3/2
  x.toString                                      //> res0: String = 1/3
  x.numer                                         //> res1: Int = 1
  x.denom                                         //> res2: Int = 3
  //x - y - z
  x < y                                           //> res3: Boolean = true
  x.max(y)                                        //> res4: week3.Rational = 5/7
  new Rational(2)                                 //> res5: week3.Rational = 2/1
  val bigR = new Rational(9999999, 87654551)      //> bigR  : week3.Rational = 9999999/87654551
  bigR.toString                                   //> res6: String = 9999999/87654551
}
class Rational(x: Int, y: Int) {
	require(y != 0, "the denominator must not be zero")
	
	def this(x: Int) = this(x, 1)
	
	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a%b)
	private val g = gcd(x, y)
 // def numer = x / g
 // def denom = y / g
	def numer = x
	def denom = y
	
  def addRational(r: Rational, s: Rational): Rational = new Rational(
      r.numer * s.denom + r.denom * s.numer,
      r.denom * s.denom)

  def + (that: Rational) = new Rational(
    numer * that.denom + denom * that.numer,
    denom * that.denom)

	def < (that: Rational) = that.denom * numer < denom * that.numer
	def max(that: Rational) = if (this < that) that else this
	
  override def toString = numer / g + "/" + denom / g

  def negRational(r: Rational) = new Rational(-r.numer, r.denom)
  def unary_- : Rational = new Rational(-numer, denom)
  def subRational(r: Rational, s: Rational) = addRational(r, negRational(s))
//  def - (that: Rational) = this + negRational(that)
  def - (that: Rational) = this + -that
}