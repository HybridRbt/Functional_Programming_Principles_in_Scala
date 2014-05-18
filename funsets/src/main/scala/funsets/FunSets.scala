package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = {
    (x: Int) => x == elem
  }

  /**
  * Returns the union of the two given sets,
  * the sets of all elements that are in either `s` or `t`.
  */
  def union(s: Set, t: Set): Set = {
    (x: Int) => contains(t, x) || contains(s, x)
  }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = {
    (x: Int) => contains(t, x) && contains(s, x)
  }

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  /**
   * Note 2 self: only the elements in 's', no elements in 't' that are not in 's'
   */
  def diff(s: Set, t: Set): Set = {
    (x: Int) => contains(s, x) && !contains(t, x)
  }

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = {
    //(x: Int) => contains(s, x) && p(x)
    (x: Int) => contains(s, x) && contains(p, x)
  }

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  /**
   * see p as another set, this translate into: if iterate through all inbound integer, every one contained in s is also contained in p, then
   * true; else false. which means s is a subset of p. thus empty s should be considered subset of p and return true.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) true /** finished iteration and no false, so true */
      else if (contains(s, a) && !contains(p, a)) false  /** when a is in bound, test to see if it is in a, and if it is, if it satisfy p */
      else iter(a-1)  /** a satisfy the req, examine the next one */
    }
    iter(bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  /**
   * see p as another set, this translate into: if there is at least one inbound integer in s that is also in p.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = ???

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = ???

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
