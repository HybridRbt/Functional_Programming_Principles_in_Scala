package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
 ignore("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  ignore("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  trait TestSets2 {
    val s1a = singletonSet(1)
    val s2a = singletonSet(2000)
    val s3a = singletonSet(30)
    val s4a = singletonSet(10)
    val s5a = singletonSet(-995)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains only 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Failed: Singleton s1 does not contains 1")
      assert(!contains(s2, 1), "Failed: Singleton s2 contains 1")
      assert(!contains(s3, 1), "Failed: Singleton s3 contains 1")
    }
  }

  test("singletonSet(2) contains only 2") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s2, 2), "Failed: Singleton s2 does not contains 2")
      assert(!contains(s1, 2), "Failed: Singleton s1 contains 2")
      assert(!contains(s3, 2), "Failed: Singleton s3 contains 2")
    }
  }

  test("singletonSet(3) contains only 3") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s3, 3), "Failed: Singleton s3 does not contains 3")
      assert(!contains(s1, 3), "Failed: Singleton s1 contains 3")
      assert(!contains(s2, 3), "Failed: Singleton s2 contains 3")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Failed: Union does not have 1")
      assert(contains(s, 2), "Failed: Union does not have 2")
      assert(!contains(s, 3), "Failed: Union has 3")
    }

    new TestSets {
      val s = union(s2, s3)
      assert(!contains(s, 1), "Failed: Union has 1")
      assert(contains(s, 2), "Failed: Union does not have 2")
      assert(contains(s, 3), "Failed: Union does not have 3")
    }

    new TestSets {
      val s = union(s1, s3)
      assert(contains(s, 1), "Failed: Union does not have 1")
      assert(!contains(s, 2), "Failed: Union has 2")
      assert(contains(s, 3), "Failed: Union does not have 3")
    }
  }

  test("intersect contains only elements in both sets") {
    new TestSets {
      val su1 = union(s1, s2)
      val su2 = union(s2, s3)
      val si = intersect(su1, su2)

      assert(contains(si, 2), "Failed: Intersect does not have 2")
      assert(!contains(si, 1), "Failed: Union has 1")
      assert(!contains(si, 3), "Failed: Union has 3")
    }

    new TestSets {
      val su1 = union(s1, s3)
      val su2 = union(s2, s3)
      val si = intersect(su1, su2)

      assert(contains(si, 3), "Failed: Intersect does not have 3")
      assert(!contains(si, 1), "Failed: Union has 1")
      assert(!contains(si, 2), "Failed: Union has 2")
    }

    new TestSets {
      val su1 = union(s1, s3)
      val su2 = union(s1, s2)
      val si = intersect(su1, su2)

      assert(contains(si, 1), "Failed: Intersect does not have 3")
      assert(!contains(si, 3), "Failed: Union has 3")
      assert(!contains(si, 2), "Failed: Union has 2")
    }
  }

  test("diff contains only elements in s and not in t") {
    new TestSets {
      val su1 = union(s1, s2)
      val su2 = union(s2, s3)
      val sd = diff(su1, su2) /** would return s1 (1) */

      assert(contains(sd, 1), "Failed: diff does not have 1")
      assert(!contains(sd, 2), "Failed: diff has 2")
      assert(!contains(sd, 3), "Failed: diff has 3")
    }

    new TestSets {
      val su1 = union(s2, s3)
      val su2 = union(s1, s3)
      val sd = diff(su1, su2) /** would return s2 (2) */

      assert(contains(sd, 2), "Failed: diff does not have 2")
      assert(!contains(sd, 1), "Failed: diff has 1")
      assert(!contains(sd, 3), "Failed: diff has 3")
    }

    new TestSets {
      val su1 = union(s1, s3)
      val su2 = union(s1, s2)
      val sd = diff(su1, su2) /** would return s3 (3) */

      assert(contains(sd, 3), "Failed: diff does not have 3")
      assert(!contains(sd, 1), "Failed: diff has 1")
      assert(!contains(sd, 2), "Failed: diff has 2")
    }
  }

  test("filter contains subset of s that satisfy p") {
    new TestSets {
      val su1 = union(s1, s2)
      val su2 = union(su1, s3) /** set == (1, 2, 3) */
      val sf = filter(su2, (x: Int) => x > 2) /** would return (3) */

      assert(contains(sf, 3), "Failed: filter does not have 3")
      assert(!contains(sf, 2), "Failed: filter has 2")
      assert(!contains(sf, 1), "Failed: filter has 1")
    }

    new TestSets {
      val su1 = union(s1, s2)
      val su2 = union(su1, s3) /** set == (1, 2, 3) */
      val sf = filter(su2, (x: Int) => x < 3) /** would return (1, 2) */

      assert(contains(sf, 1), "Failed: filter does not have 1")
      assert(contains(sf, 2), "Failed: filter does not have 2")
      assert(!contains(sf, 3), "Failed: filter has 3")
    }

    new TestSets {
      val su1 = union(s1, s2)
      val su2 = union(su1, s3) /** set == (1, 2, 3) */
      val sf = filter(su2, (x: Int) => x > 3) /** would return (*empty*) */

      assert(!contains(sf, 1), "Failed: filter has 1")
      assert(!contains(sf, 2), "Failed: filter has 2")
      assert(!contains(sf, 3), "Failed: filter has 3")
    }

    new TestSets {
      val su1 = union(s1, s2)
      val su2 = union(su1, s3) /** set == (1, 2, 3) */
      val sf = filter(su2, (x: Int) => x > 0) /** would return (1, 2, 3) */

      assert(contains(sf, 1), "Failed: filter does not have 1")
      assert(contains(sf, 2), "Failed: filter does not have 2")
      assert(contains(sf, 3), "Failed: filter does not have 3")
    }
  }

  test("forall tests all integer in bound = +/- 1000") {
    new TestSets2 {
      val su1 = union(s1a, s2a)
      /** set == (1, 1000) */
      val su2 = union(su1, s3a)
      /** set == (1, 1000, 30) */
      val su3 = union(su2, s4a)
      /** set == (1, 1000, 30, 10) */
      val su4 = union(su3, s5a)
      /** set == (1, 1000, 30, 10, -995) */
      val sfa1 = forall(su4, (x: Int) => x > 2)
      /** would return false */
      val sfa2 = forall(su4, (x: Int) => x < 0)
      /** would return false */
      val sfa3 = forall(su4, (x: Int) => x > -999 && x < 2000) /** would return true */

      assert(!sfa1, "Failed: forall should return false")
      assert(!sfa2, "Failed: forall should return false")
      assert(sfa3, "Failed: forall should return true")
    }
  }
}
