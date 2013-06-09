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
  test("string take") {
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
  test("adding ints") {
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
    def emptySet: Set = x => false
    def boundedSet(left: Int, right: Int): Set = x => (x >= left) && (x <= right)
    def randomSet(ll: List[Int]): Set = x => ll.contains(x)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("union with empty set") {
    new TestSets {
      val s = union(s1, emptySet)
      assert(contains(s, 1), "Union 1")
      assert(!contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersect singletons") {
    new TestSets {
      val s = intersect(s1, s2)
      val s0 = intersect(s1, emptySet)
      assert(!contains(s, 1), "intersect 1")
      assert(!contains(s, 2), "intersect 2")
      assert(FunSets.toString(s) == FunSets.toString(emptySet), "s1 intersect s2 is empty")
      assert(!contains(s0, 1), "intersect 1 and null")
      assert(FunSets.toString(s0) == FunSets.toString(emptySet), "s1 intersect empty is empty")
    }
  }
  
  test("diff two sets") {
    new TestSets {
      val s = randomSet(List(1, 2, 6, 9, -3, 0, -85))
      val ds1 = diff(s, randomSet(List(1, 2, 6, 9)))
      val ds2 = diff(s, randomSet(List(1, 2, 6, 9, -3, 0, -85)))
      val ds3 = diff(s, emptySet)
      val ds4 = diff(emptySet, s1)
      assert(contains(ds1, 0) && contains(ds1, -3) && contains(ds1, -85), "nonpositive left")
      assert(!contains(ds1, 1) && !contains(ds1, 2) && !contains(ds1, 6), "no positive left")
      assert(!contains(ds2, 0) && !contains(ds2, -3) && !contains(ds2, 2), "nothing left")
      assert(contains(ds3, 1) && contains(ds3, 0) && contains(ds3, -85), "everything left")
      assert(!contains(ds4, 9) && !contains(ds4, 0) && !contains(ds4, -3), "still nothing in the set")
      assert(FunSets.toString(ds1) == FunSets.toString(randomSet(List(-3, 0, -85))), "all less than zero")
      assert(FunSets.toString(ds2) == FunSets.toString(emptySet), "empty")
      assert(FunSets.toString(ds3) == FunSets.toString(s), "no change")
      assert(FunSets.toString(ds4) == FunSets.toString(emptySet), "nothing to get rid of")
    }
  }
  
  test("filter out numbers") {
    new TestSets {
      val s = filter(boundedSet(-7, 5), x => x % 2 == 0)
      assert(forall(s, x => x % 2 == 0), "all even numbers")
      assert(forall(s, x => x >= -7 && x <= 5), "between -7 and 5")
      assert(exists(s, x => x < -2), "one element is less than -2")
      assert(FunSets.toString(filter(randomSet(List(1, 2, 3, 4, 5, 7, 1000)), _ < 5)) == FunSets.toString(randomSet(List(1, 2, 3, 4))), "less than 5")
    }
  }
  
  test("Queries") {
    new TestSets {
      assert(forall(x => false, x => false), "empty to empty")
      assert(forall(x => false, x => true), "empty to full")
      assert(forall(randomSet(List(1, 3, 5, 101, 999)), x => x % 2 == 1), "odd numbers")
      assert(!forall(randomSet(List(1, 3, 5, 101, 999)), x => x % 2 == 0), "still odd numbers")
      assert(forall(map(randomSet(List(1, 3, 5, 101, 999)), x => x + 1), x => x % 2 == 0), "even numbers")
      assert(FunSets.toString(map(randomSet(List(1, 3, 5, 101, 999)), x => x + 1)) == FunSets.toString(randomSet(List(2, 4, 6, 102, 1000))), "add one")
      assert(exists(map(randomSet(List(0, 3, 5, 101, 999)), x => x + 1), x => x % 2 == 1), "at least one odd number")
    }
  }

}
