package scala.collection.parallel
package scalatest

import org.scalatest._
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._
import Par._
import workstealing.Ops._

class RangeTest extends FunSuite with Timeouts {

  implicit val scheduler = new workstealing.WorkstealingTreeScheduler.ForkJoin()

  def runForSizes(method: Int => Unit) {
    for (i <- 1 to 1000) {
      method(i)
    }
    for (i <- 1000 to 10000 by 1000) {
      method(i)
    }
    for (i <- 10000 to 100000 by 10000) {
      method(i)
    }
    for (i <- 100000 to 1000000 by 100000) {
      method(i)
    }
    for (i <- 1000000 to 10000000 by 1000000) {
      method(i)
    }
  }

  def testReduce(sz: Int): Unit = try {
    failAfter(1 seconds) {
      val r = 0 until sz
      val x = r.reduce(_ + _)

      val pr = r.toPar
      val px = pr.reduce(_ + _)

      assert(x == px, x + ", " + px)
    }
  } catch {
    case e: exceptions.TestFailedDueToTimeoutException =>
      assert(false, "timeout for size: " + sz)
  }

  test("reduce") {
    intercept[UnsupportedOperationException] {
      testReduce(0)
    }
    runForSizes(testReduce)
  }

  def testFold(sz: Int): Unit = try {
    failAfter(1 seconds) {
      val r = 0 until sz
      val x = r.fold(0)(_ + _)

      val pr = r.toPar
      val px = pr.fold(0)(_ + _)

      assert(x == px, x + ", " + px)
    }
  } catch {
    case e: exceptions.TestFailedDueToTimeoutException =>
      assert(false, "timeout for size: " + sz)
  }

  test("fold") {
    testFold(0)
    runForSizes(testFold)
  }

  def testAggregate(sz: Int): Unit = try {
    failAfter(1 seconds) {
      val r = 0 until sz
      val x = r.aggregate(0)(_ + _, _ + _)

      val pr = r.toPar
      val px = pr.aggregate(0)(_ + _)(_ + _)

      assert(x == px, x + ", " + px)
    }
  } catch {
    case e: exceptions.TestFailedDueToTimeoutException =>
      assert(false, "timeout for size: " + sz)
  }

  test("aggregate") {
    testAggregate(0)
    runForSizes(testAggregate)
  }

  def testSum(sz: Int): Unit = try {
    failAfter(1 seconds) {
      val r = 0 until sz
      val x = r.sum

      val pr = r.toPar
       val px = pr.sum

      assert(x == px, x + ", " + px)
    }
  } catch {
    case e: exceptions.TestFailedDueToTimeoutException =>
      assert(false, "timeout for size: " + sz)
  }

  test("sum") {
    testSum(0)
    runForSizes(testSum)
  }

  def testProduct(sz: Int): Unit = try {
    failAfter(1 seconds) {
      val r = 0 until sz
      val x = r.product

      val pr = r.toPar
      val px = pr.product

      assert(x == px, x + ", " + px)
    }
  } catch {
    case e: exceptions.TestFailedDueToTimeoutException =>
      assert(false, "timeout for size: " + sz)
  }

  test("product") {
    testSum(0)
    runForSizes(testProduct)
  }

  def testMin(sz: Int): Unit = try {
    failAfter(1 seconds) {
      val r = 0 until sz
      val x = r.min

      val pr = r.toPar
      val px = pr.min

      assert(x == px, x + ", " + px)
    }
  } catch {
    case e: exceptions.TestFailedDueToTimeoutException =>
      assert(false, "timeout for size: " + sz)
  }

  test("min") {
    /*  intercept[UnsupportedOperationException] {
      testMin(0)
    } */

    runForSizes(testMin)
  }

  def testMax(sz: Int): Unit = try {
    failAfter(1 seconds) {
      val r = 0 until sz
      val x = r.max

      val pr = r.toPar
      val px = pr.max

      assert(x == px, x + ", " + px)
    }
  } catch {
    case e: exceptions.TestFailedDueToTimeoutException =>
      assert(false, "timeout for size: " + sz)
  }

  test("max") {
    /*  intercept[UnsupportedOperationException] {
      testMax(0)
    } */
    runForSizes(testMax)
  }

}

