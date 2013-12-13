package org.scala.optimized.test.par
package scalameter


import scala.collection.optimizer._
import org.scalameter.api._


/* this class requires java8 and the mx utility from graal to be avalilable  to be avaliable in $PATH */
class JVMBench extends PerformanceTest.Regression with Serializable with Generators {

  /* config */

  def persistor = new SerializationPersistor



  val opts = Seq(
    exec.minWarmupRuns -> 50,
    exec.maxWarmupRuns -> 100,
    exec.benchRuns -> 30,
    exec.independentSamples -> 1,
    exec.jvmflags -> "-Xms3072m -Xmx3072m -XX:MaxPermSize=256m -XX:ReservedCodeCacheSize=64m -XX:+UseCondCardMark -XX:CompileThreshold=100 -Dscala.collection.parallel.range.manual_optimizations=true",
    reports.regression.noiseMagnitude -> 0.15)

  val java8 = opts ++ Seq(exec.jvmcmd -> "java8 -server")
  val graal = opts ++ Seq(exec.jvmcmd -> "mx --vm graal vm", 
    exec.jvmflags -> "-Xms3072m -Xmx3072m -XX:ReservedCodeCacheSize=64m -Dscala.collection.parallel.range.manual_optimizations=true")

  val pcopts = Seq(
    exec.minWarmupRuns -> 2,
    exec.maxWarmupRuns -> 4,
    exec.benchRuns -> 4,
    exec.independentSamples -> 1,
    reports.regression.noiseMagnitude -> 0.75)

  /* benchmarks */

  performance of "Optimized[Array]" config (opts: _*) in {

    val small = 250000
    val large = 1500000

    measure method "reduce" in {
      using(arrays(large)) curve ("collections7") in { x => x.reduce(_ + _) }
      using(arrays(large)) curve ("optimized7") in { x => optimize{x.reduce(_ + _)} }
      using(arrays(large)) curve ("collections8") config (java8: _*) in { x => x.reduce(_ + _) }
      using(arrays(large)) curve ("optimized8") config (java8: _*) in { x => optimize{x.reduce(_ + _)} }
      using(arrays(large)) curve ("collectionsG") config (graal: _*) in { x => x.reduce(_ + _) }
      using(arrays(large)) curve ("optimizedG") config (graal: _*) in { x => optimize{x.reduce(_ + _)} }
    }



    measure method "map" in {
      using(arrays(small)) curve ("collections7") in { x => x.map(x => x + 1) }
      using(arrays(small)) curve ("optimized7") in { x => optimize{x.map(x => x + 1)} }
      using(arrays(small)) curve ("collections8") config (java8: _*) in { x => x.map(x => x + 1) }
      using(arrays(small)) curve ("optimized8") config (java8: _*) in { x => optimize{x.map(x => x + 1)} }
      using(arrays(small)) curve ("collectionsG") config (graal: _*) in { x => x.map(x => x + 1) }
      using(arrays(small)) curve ("optimizedG") config (graal: _*) in { x => optimize{x.map(x => x + 1)} }
    }

    measure method "find" in {
      using(arrays(large)) curve ("collections7") in { x => val result: Option[Int] = x.find(_ < -1); result}
      using(arrays(large)) curve ("optimized7") in { x => val result: Option[Int] = optimize{x.find(_ < -1)}; result }
      using(arrays(large)) curve ("collections8") config (java8: _*) in { x => val result: Option[Int] = x.find(_ < -1); result}
      using(arrays(large)) curve ("optimized8") config (java8: _*) in { x => val result: Option[Int] = optimize{x.find(_ < -1)}; result }
      using(arrays(large)) curve ("collectionsG") config (graal: _*) in { x => val result: Option[Int] = x.find(_ < -1); result}
      using(arrays(large)) curve ("optimizedG") config (graal: _*) in { x => val result: Option[Int] = optimize{x.find(_ < -1)}; result }
    }

    measure method "flatMap" in {
      using(arrays(small)) curve ("collections7") in { x => x.flatMap(x => List(x, x, x)) }
      using(arrays(small)) curve ("optimized7") in { x => optimize{x.flatMap(x => List(x, x, x))} }
      using(arrays(small)) curve ("collections8") config (java8: _*) in { x => x.flatMap(x => List(x, x, x)) }
      using(arrays(small)) curve ("optimized8") config (java8: _*) in { x => optimize{x.flatMap(x => List(x, x, x))} }
      using(arrays(small)) curve ("collectionsG") config (graal: _*) in { x => x.flatMap(x => List(x, x, x)) }
      using(arrays(small)) curve ("optimizedG") config (graal: _*) in { x => optimize{x.flatMap(x => List(x, x, x))} }
    }
  }

  performance of "Optimized[Range]" config (opts: _*) in {
    val tiny = 300000
    val small = 3000000
    val large = 30000000

    measure method "reduce" in {
      using(ranges(large)) curve ("collections7") in { x => x.reduce(_ + _) }
      using(ranges(large)) curve ("optimized7") in { x => optimize{x.reduce(_ + _)} }
      using(ranges(large)) curve ("collections8") config (java8: _*) in { x => x.reduce(_ + _) }
      using(ranges(large)) curve ("optimized8") config (java8: _*) in { x => optimize{x.reduce(_ + _)} }
      using(ranges(large)) curve ("collectionsG") in { x => x.reduce(_ + _) }
      using(ranges(large)) curve ("optimizedG") in { x => optimize{x.reduce(_ + _)} }
    }

    measure method "map" in {
      using(ranges(small)) curve ("collections7") in { x => x.map(x => x + 1)}
      using(ranges(small)) curve ("optimized7") in { x => optimize{x.map(x => x + 1)} }
      using(ranges(small)) curve ("collections8") config (java8: _*) in { x => x.map(x => x + 1)}
      using(ranges(small)) curve ("optimized8") config (java8: _*) in { x => optimize{x.map(x => x + 1)} }
      using(ranges(small)) curve ("collectionsG") config (graal: _*) in { x => x.map(x => x + 1)}
      using(ranges(small)) curve ("optimizedG") config (graal: _*) in { x => optimize{x.map(x => x + 1)} }
    }

    measure method "find" in {
      using(ranges(large)) curve ("collections7") in { x => x.find(_ < Int.MinValue) }
      using(ranges(large)) curve ("optimized7") in { x => optimize{x.find(_ < Int.MinValue)} }
      using(ranges(large)) curve ("collections8") config (java8: _*) in { x => x.find(_ < Int.MinValue) }
      using(ranges(large)) curve ("optimized8") config (java8: _*) in { x => optimize{x.find(_ < Int.MinValue)} }
      using(ranges(large)) curve ("collectionsG") config (graal: _*) in { x => x.find(_ < Int.MinValue) }
      using(ranges(large)) curve ("optimizedG") config (graal: _*) in { x => optimize{x.find(_ < Int.MinValue)} }
    }

    measure method "flatMap" in {
      using(ranges(small)) curve ("collections7") in { x => x.flatMap(x => List(x, x, x)) }
      using(ranges(small)) curve ("optimized7") in { x => optimize{x.flatMap(x => List(x, x, x))} }
      using(ranges(small)) curve ("collections8") config (java8: _*) in { x => x.flatMap(x => List(x, x, x)) }
      using(ranges(small)) curve ("optimized8") config (java8: _*) in { x => optimize{x.flatMap(x => List(x, x, x))} }
      using(ranges(small)) curve ("collectionsG") config (graal: _*) in { x => x.flatMap(x => List(x, x, x)) }
      using(ranges(small)) curve ("optimizedG") config (graal: _*) in { x => optimize{x.flatMap(x => List(x, x, x))} }
    }

    measure method "aggregate(tiny)" in {
      using(ranges(tiny)) curve ("collections7") in { x =>
        var sum = 0
        var i = x.head
        val until = x.end
        while(i < until) {
          sum = sum + (1 to 13).aggregate(0)(_ + _, _ + _)
          i = i + 1
        }
      }
      using(ranges(tiny)) curve ("optimized7") in { x => 
        optimize{
          var sum = 0
          var i = x.head
          val until = x.end
          while(i < until) {
            sum = sum + (1 to 13).aggregate(0)(_ + _, _ + _)
            i = i + 1
          }
        }
      }
      using(ranges(tiny)) curve ("collections8") config (java8: _*) in { x =>
        var sum = 0
        var i = x.head
        val until = x.end
        while(i < until) {
          sum = sum + (1 to 13).aggregate(0)(_ + _, _ + _)
          i = i + 1
        }
      }
      using(ranges(tiny)) curve ("optimized8") config (java8: _*) in { x => 
        optimize{
          var sum = 0
          var i = x.head
          val until = x.end
          while(i < until) {
            sum = sum + (1 to 13).aggregate(0)(_ + _, _ + _)
            i = i + 1
          }
        }
      }
      using(ranges(tiny)) curve ("collectionsG") config (graal: _*) in { x =>
        var sum = 0
        var i = x.head
        val until = x.end
        while(i < until) {
          sum = sum + (1 to 13).aggregate(0)(_ + _, _ + _)
          i = i + 1
        }
      }
      using(ranges(tiny)) curve ("optimizedG") config (graal: _*) in { x => 
        optimize{
          var sum = 0
          var i = x.head
          val until = x.end
          while(i < until) {
            sum = sum + (1 to 13).aggregate(0)(_ + _, _ + _)
            i = i + 1
          }
        }
      }
    }

    measure method "ProjectEuler1" in {

      using(ranges(small)) curve ("collections7") in { x =>
        x.filter(x => (x % 3 == 0)|| (x % 5 == 0)).reduce(_ + _)
      }
      using(ranges(small)) curve ("optimized7") in { x => 
        optimize{x.filter(x => (x % 3 == 0)|| (x % 5 == 0)).reduce(_ + _)}
      }
      using(ranges(small)) curve ("collections8") config (java8: _*) in { x =>
        x.filter(x => (x % 3 == 0)|| (x % 5 == 0)).reduce(_ + _)
      }
      using(ranges(small)) curve ("optimized8") config (java8: _*) in { x => 
        optimize{x.filter(x => (x % 3 == 0)|| (x % 5 == 0)).reduce(_ + _)}
      }
      using(ranges(small)) curve ("collectionsG") config (graal: _*) in { x =>
        x.filter(x => (x % 3 == 0)|| (x % 5 == 0)).reduce(_ + _)
      }
      using(ranges(small)) curve ("optimizedG") config (graal: _*) in { x => 
        optimize{x.filter(x => (x % 3 == 0)|| (x % 5 == 0)).reduce(_ + _)}
      }
    }

  }
}

