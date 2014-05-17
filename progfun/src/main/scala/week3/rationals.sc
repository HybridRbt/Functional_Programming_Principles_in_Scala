object rationals {
  val x = new Rational(1, 2)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x.numer
  x.denom
  x.sub(y).sub(z)
}
class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def addRational(r: Rational, s: Rational): Rational = new Rational(
      r.numer * s.denom + r.denom * s.numer,
      r.denom * s.denom)

  def add(that: Rational) = new Rational(
    numer * that.denom + denom * that.numer,
    denom * that.denom)

  def makeString(r: Rational) =
  r.numer + "/" + r.denom

  def negRational(r: Rational) = new Rational(-r.numer, r.denom)
  def neg: Rational = new Rational(-numer, denom)
  def subRational(r: Rational, s: Rational) = addRational(r, negRational(s))
  def sub(that: Rational) = add(negRational(that))
  def sub2(that: Rational) = add(that.neg)
}