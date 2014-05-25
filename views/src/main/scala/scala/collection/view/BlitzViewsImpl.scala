package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell
import scala.collection.par.generic.IsReducable
import scala.reflect.ClassTag
import scala.collection.par.workstealing.Arrays.Array2ZippableConvertor
import scala.annotation.implicitNotFound
import scala.collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}
import scala.collection.immutable.{HashSet, HashMap}

trait ViewTransform[-A, +B] { self =>
  type Fold[A, F] = (A, ResultCell[F]) => ResultCell[F]

  def fold[F](g: Fold[B, F]): Fold[A, F]

  def >>[C](next: ViewTransform[B, C]) = new ViewTransform[A, C] {
    def fold[F](fd: Fold[C, F]): Fold[A, F] = self.fold(next.fold(fd))
  }
}

object ViewTransforms {
  class Identity[A] extends ViewTransform[A, A] {
    def fold[F](fd: Fold[A, F]): Fold[A, F] = fd
  }

  class Map[A, B](m: A => B) extends ViewTransform[A, B] {
    def fold[F](fd: Fold[B, F]): Fold[A, F] =
      (x, acc) => fd(m(x), acc)
  }

  class Filter[A](p: A => Boolean) extends ViewTransform[A, A] {
    def fold[F](fd: Fold[A, F]): Fold[A, F] =
      (x, acc) => if (p(x)) fd(x, acc) else acc
  }
}

trait BlitzViewImpl[B] extends BlitzView[B] { self =>
  /* internals */
  def >>[C](next: ViewTransform[B, C]): BlitzViewImpl[C]
  def aggInternal[R](z: => R)(op: (B, R) => R, stopper: ResultCell[R] => Boolean)(reducer: (R, R) => R)(implicit ctx: Scheduler): R

  /* methods: V -> V */
  override def drop(n: Int): BlitzView[B] = ???
  override def take(n: Int): BlitzView[B] = ???

  /* methods: V -> other array structure */
  override def toArray()(implicit classtag: ClassTag[B], ctx: Scheduler): Array[B] = {
    val tmp = toList_().toArray
    val sz = tmp.size - 1
    if (sz < 0) return tmp
    val rng = 0 to (sz / 2)
    def swap(x: Int) = {
      val t = tmp(x)
      tmp(x) = tmp(sz - x)
      tmp(sz - x) = t
    }
    rng.toPar.foreach(swap(_))
    tmp
  }

  private[this] def toList_()(implicit ctx: Scheduler): List[B] =
    aggregate(Nil: List[B])((x, xs) => x :: xs)((xs, ys) => ys ++ xs)

  override def toList()(implicit ctx: Scheduler): List[B] =
    toList_().reverse

  /* methods: V -> V[constant type] */
  override def toInts(implicit f: Numeric[B]): BlitzView[Int] =
    map(f.toInt(_))
  override def toDoubles(implicit f: Numeric[B]): BlitzView[Double] =
    map(f.toDouble(_))
  override def toFloats(implicit f: Numeric[B]): BlitzView[Float] =
    map(f.toFloat(_))
  override def toLongs(implicit f: Numeric[B]): BlitzView[Long] =
    map(f.toLong(_))

  /* methods: V -> 1 */
  override def reduce(op: (B, B) => B)(implicit ctx: Scheduler): B =
    reduceOpt(op)(ctx).get // throws an Exception if empty
  override def min()(implicit ord: Ordering[B], ctx: Scheduler): B =
    minOpt()(ord, ctx).get // throws an Exception if empty
  override def max()(implicit ord: Ordering[B], ctx: Scheduler): B =
    maxOpt()(ord, ctx).get // throws an Exception if empty
  override def sum()(implicit num: Numeric[B], ctx: Scheduler): B =
    aggregate(num.zero)(num.plus(_, _))(num.plus(_, _))
  override def product()(implicit num: Numeric[B], ctx: Scheduler): B =
    aggregate(num.one)(num.times(_, _))(num.times(_, _))
}

object View {
  def apply[T, Repr](xss: Par[Repr])(implicit conv: IsReducable[Repr, T]): BlitzView[T] = new BlitzViewC[T] {
    type A = T
    val xs = conv(xss)
    def transform = new ViewTransforms.Identity()
  }

  def apply[T, T1 <: T, T2 <: T](xss: BlitzView[T1], yss: BlitzView[T2]): BlitzView[T] = new BlitzViewVV[T] {
    type A = T
    val xs = xss.asInstanceOf[BlitzViewImpl[T]]
    val ys = yss.asInstanceOf[BlitzViewImpl[T]]
    def transform = new ViewTransforms.Identity()
  }

  def range(x: Int, y: Int)(implicit ctx: Scheduler): BlitzView[Int] = {
    val xs = x until (y+1)
    apply(xs.toPar)(rangeIsZippable(ctx))
  }

  def of[T](xss: T*)(implicit conv: IsReducable[Array[T], T], c: ClassTag[T]): BlitzView[T] = new BlitzViewC[T] {
    type A = T
    val xs = conv(xss.toArray.toPar)
    def transform = new ViewTransforms.Identity()
  }
}

object ViewUtils {
  // equivalent to liftA2 for the Option[_] Functor
  def optCombine[A](f: (A, A) => A)(ox: Option[A], oy: Option[A]): Option[A] =
    (ox, oy) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => ox.orElse(oy)
    }

  def rcCombine[A](f: (A, A) => A)(ox: ResultCell[A], oy: ResultCell[A]): ResultCell[A] =
    {
      if (!ox.isEmpty && !oy.isEmpty) {
        val rc = new ResultCell[A]
        rc.result = f(ox.result, oy.result)
        rc
      } else {
        if (ox.isEmpty) oy else ox
      }
    }

  def toStopper[T,R](pstop: ResultCell[R] => Boolean)
    (x: T, rc: ResultCell[R]): Boolean = pstop(rc)
  def neverStop[T,R](rc: ResultCell[R]): Boolean = false
  def nonEmptyStop[T,R](rc: ResultCell[R]): Boolean = !rc.isEmpty
  def equalStop[T,R](v: R)(rc: ResultCell[R]): Boolean =
    !rc.isEmpty && rc.result == v
}

/*
 * We provide implicit conversions for certain native types. The user can call
 * bview on the supported classes, the implicit IsViewable for this type kicks
 * in and attach the bview method to it by converting the type to Viewable.
 *
 * As follows: toViewable[L, A] -> search for an implicit IsViewable[L, A]
 * then: get the appropriate implicit, for eg: arrayIsViewable
 * this requires an implicit Scheduler in scope, given this one, we can create
 * an IsViewable instance that can be used as an evidence applied in toViewable.
 *
 * toViewable <- arrayIsViewable (<- Scheduler)
 *  \-> IsViewable -> Viewable with bview
 */
object Scope {
  implicit def toViewable[L, A](col: L)(implicit evidence: IsViewable[L, A]) =
    new Viewable[L, A](col)(evidence)

  @implicitNotFound("cannot find a valid conversion from ${L} to BlitzView")
  trait IsViewable[L, A] {
    def apply(c: L): BlitzView[A]
  }

  class Viewable[L, A](col: L)(evidence: IsViewable[L, A]) {
    def bview = evidence.apply(col)
  }

  /* Specialized simple data lists */

  implicit def arrayIsViewable[T](implicit ctx: Scheduler) =
    new IsViewable[Array[T], T] {
      override def apply(c: Array[T]): BlitzView[T] = {
        View(c.toPar)(new Array2ZippableConvertor)
      }
    }

  implicit def rangeIsViewable[L <: Range](implicit ctx: Scheduler) =
    new IsViewable[L, Int] {
      override def apply(c: L): BlitzView[Int] = {
        View(c.toPar)(rangeIsZippable)
      }
    }


  /* Specialized faster Set/Map implementation */

  implicit def hashsetIsViewable[T](implicit ctx: Scheduler, ct: ClassTag[T]) =
    new IsViewable[HashSet[T], T] {
      override def apply(c: HashSet[T]): BlitzView[T] = {
        View(c.toPar)(hashTrieSetIsReducable)
      }
    }

  implicit def hashmapIsViewable[K,V](implicit ctx: Scheduler, ct1: ClassTag[Tuple2[K,V]]) =
    new IsViewable[HashMap[K,V], (K,V)] {
      override def apply(c: HashMap[K,V]): BlitzView[(K,V)] = {
        View(c.toPar)(hashTrieMapIsReducable)
      }
    }


  /* More general slower Set/Map implementation */

  // TODO: should we convert to Array, for immutable, stays correct
  // Or we specialize with hashSetIsReducable (cf: workstealing)
  // TODO: how to factorize with arrayIsViewable?
  implicit def setIsViewable[T](implicit ctx: Scheduler, ct: ClassTag[T]) =
    new IsViewable[Set[T], T] {
      override def apply(c: Set[T]): BlitzView[T] = {
        View(c.toArray.toPar)(new Array2ZippableConvertor)
      }
    }

  // TODO: same as setIsViewable
  implicit def mapIsViewable[K,V](implicit ctx: Scheduler, ct1: ClassTag[Tuple2[K,V]]) =
    new IsViewable[Map[K,V], (K,V)] {
      override def apply(c: Map[K,V]): BlitzView[(K,V)] = {
        View(c.toArray.toPar)(new Array2ZippableConvertor)
      }
    }


  /* Mutable Set/Map data structures */

  implicit def mHashSetIsViewable[T](implicit ctx: Scheduler) =
    new IsViewable[MHashSet[T], T] {
      override def apply(c: MHashSet[T]): BlitzView[T] = {
        View(c.toPar)(hashSetIsReducable)
      }
    }

  implicit def mHashMapIsViewable[K,V](implicit ctx: Scheduler) =
    new IsViewable[MHashMap[K,V], (K,V)] {
      override def apply(c: MHashMap[K,V]): BlitzView[(K,V)] = {
        View(c.toPar)(hashMapIsReducable)
      }
    }


  /* Provides 'flatten' on nested Views */

  implicit def addFlatten[U, B <: BlitzView[BlitzView[U]]](view: B)(implicit ctx: Scheduler, ct: ClassTag[U]) =
    new ViewWithFlatten[U, B](view)(ctx, ct)

  class ViewWithFlatten[U, B <: BlitzView[BlitzView[U]]](val view: B)(implicit ctx: Scheduler, ct: ClassTag[U]) extends AnyVal {
    def flatten: BlitzView[U] = {
      def nArray = new Array(0) // FIXME: create a new one each time?
      val xs = view.map{ _.toArray }.aggregate(nArray)(_ ++ _)(_ ++ _)
      xs.toArray.bview
    }
  }
}

