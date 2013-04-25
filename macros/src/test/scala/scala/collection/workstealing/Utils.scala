package scala.collection.workstealing

import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.scalameter.api._

object UnsafeOverheadBenchmark extends PerformanceTest {
    import Executor.Measurer
    def warmer = Executor.Warmer.Default()
    def aggregator = Aggregator.min
    def measurer = new Measurer.IgnoringGC with Measurer.PeriodicReinstantiation {
      override val defaultFrequency = 12
      override val defaultFullGC = true
    }
    def executor = SeparateJvmsExecutor(warmer, aggregator, measurer)

  def reporter: Reporter = new HtmlReporter(HtmlReporter.Renderer.basic: _*)
  def persistor = Persistor.None

  class TestClass(@volatile var int: Int = 1, @volatile var long: Long = 2, @volatile var double: Double = 3, @volatile var boolean: Boolean = false, @volatile var byte: Byte = 5, @volatile var char: Char = 'c', @volatile var obj: AnyRef = Nil, @volatile var short: Short = 6, @volatile var float:Float = 7)

  val sizes = Gen.enumeration("size")(1000, 5000, 20000)
  val testClasses = for (size <- sizes) yield Array.fill(size) { new TestClass() }

  performance of "unsafe overhead" config (exec.independentSamples -> 4, exec.benchRuns -> 50, exec.jvmflags -> "-Xmx512m -Xms512m") in {
    measure method "time" in {
      using(testClasses) curve "unsafeGetIntV" in {
        array =>
          var acc = 0
          array.foreach { x => acc += Utils.readVolatile(x.int) }
      }

      using(testClasses) curve "GetIntV" in {
        array =>
          var acc = 0
          array.foreach { x => acc += x.int }
      }
      using(testClasses) curve "unsafeGetLongV" in {
        array =>
          var acc = 0L
          array.foreach { x => acc += Utils.readVolatile(x.long) }
      }

      using(testClasses) curve "GetLongV" in {
        array =>
          var acc = 0L
          array.foreach { x => acc += x.long }
      }

      using(testClasses) curve "unsafeGetCharV" in {
        array =>
          var acc = 0
          array.foreach { x => acc += Utils.readVolatile(x.char) }
      }

      using(testClasses) curve "GetCharV" in {
        array =>
          var acc = 0
          array.foreach { x => acc += x.char }
      }
      using(testClasses) curve "unsafeGetByteV" in {
        array =>
          var acc = 0
          array.foreach { x => acc += Utils.readVolatile(x.byte) }
      }

      using(testClasses) curve "GetByteV" in {
        array =>
          var acc = 0
          array.foreach { x => acc += x.byte }
      }

      using(testClasses) curve "unsafeGetShortV" in {
        array =>
          var acc = 0
          array.foreach { x => acc += Utils.readVolatile(x.short) }
      }

      using(testClasses) curve "GetShortV" in {
        array =>
          var acc = 0
          array.foreach { x => acc += x.short }
      }

      using(testClasses) curve "unsafeGetFloatV" in {
        array =>
          var acc = 0.0
          array.foreach { x => acc += Utils.readVolatile(x.float) }
      }

      using(testClasses) curve "GetFloatV" in {
        array =>
          var acc = 0.0
          array.foreach { x => acc += x.float }
      }

      using(testClasses) curve "unsafeGetDoubleV" in {
        array =>
          var acc = 0.0
          array.foreach { x => acc += Utils.readVolatile(x.double) }
      }

      using(testClasses) curve "GetDoubleV" in {
        array =>
          var acc = 0.0
          array.foreach { x => acc += x.double }
      }

      using(testClasses) curve "unsafeGetBooleanV" in {
        array =>
          var acc = true
        array.foreach { x => acc = acc&& Utils.readVolatile(x.boolean) }
      }

      using(testClasses) curve "GetBooleanV" in {
        array =>
          var acc = true
          array.foreach { x => acc = acc && x.boolean }
      }
      using(testClasses) curve "unsafeGetObjectV" in {
        array =>
          var acc:Any = Nil
          array.foreach { x => acc = Utils.readVolatile(x.obj) }
      }

      using(testClasses) curve "GetObjectV" in {
        array =>
          var acc = 0
          array.foreach { x => acc += x.int }
      }



    }
  }
}
