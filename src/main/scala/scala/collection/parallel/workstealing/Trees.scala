package scala.collection.parallel
package workstealing



import scala.language.experimental.macros
import scala.reflect.macros._
import scala.reflect.ClassTag
import scala.collection.parallel.generic._
import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap
import scala.collection.immutable.TrieIterator



object Trees {

  import WorkstealingTreeScheduler.{ Kernel, Node }

  val TRIE_ITERATOR_NAME = classOf[TrieIterator[_]].getName.replace(".", "$")
  val HASHSET1_NAME = classOf[TrieIterator[_]].getName.replace(".", "$")

  def mangledT(x: String) = TRIE_ITERATOR_NAME + "$$" + x
  def mangledHS(x: String) = TRIE_ITERATOR_NAME + "$$" + x

  val OFFSET_DEPTH = unsafe.objectFieldOffset(classOf[TrieChunkIterator[_, _]].getField(mangledT("depth")))
  val OFFSET_ARRAY_STACK = unsafe.objectFieldOffset(classOf[TrieChunkIterator[_, _]].getField(mangledT("arrayStack")))
  val OFFSET_POS_STACK = unsafe.objectFieldOffset(classOf[TrieChunkIterator[_, _]].getField(mangledT("posStack")))
  val OFFSET_ARRAY_D = unsafe.objectFieldOffset(classOf[TrieChunkIterator[_, _]].getField(mangledT("arrayD")))
  val OFFSET_POS_D = unsafe.objectFieldOffset(classOf[TrieChunkIterator[_, _]].getField(mangledT("posD")))
  val OFFSET_SUBITER = unsafe.objectFieldOffset(classOf[TrieChunkIterator[_, _]].getField(mangledT("subIter")))
  val OFFSET_HS_KEY = unsafe.objectFieldOffset(classOf[HashSet.HashSet1[_]].getDeclaredField("key"))
  val OFFSET_HM_KV = unsafe.objectFieldOffset(classOf[HashMap.HashMap1[_, _]].getDeclaredField("kv"))
  val OFFSET_HM_KEY = unsafe.objectFieldOffset(classOf[HashMap.HashMap1[_, _]].getDeclaredField("key"))
  val OFFSET_HM_VALUE = unsafe.objectFieldOffset(classOf[HashMap.HashMap1[_, _]].getDeclaredField("value"))

  final def key[T](hs1: HashSet.HashSet1[T]) = unsafe.getObject(hs1, OFFSET_HS_KEY).asInstanceOf[T]

  final def kv[K, V](hm1: HashMap.HashMap1[K, V]): (K, V) = {
    val kv = unsafe.getObject(hm1, OFFSET_HM_KV)
    if (kv == null) (unsafe.getObject(hm1, OFFSET_HM_KEY).asInstanceOf[K], unsafe.getObject(hm1, OFFSET_HM_VALUE).asInstanceOf[V])
    else kv.asInstanceOf[(K, V)]
  }

  abstract class TrieChunkIterator[T, Repr] extends TrieIterator[T](null) with TreeStealer.ChunkIterator[T] {
    final def setDepth(d: Int) = unsafe.putInt(this, OFFSET_DEPTH, d)
    final def setPosD(p: Int) = unsafe.putInt(this, OFFSET_POS_D, p)
    final def setArrayD(a: Array[Iterable[T]]) = unsafe.putObject(this, OFFSET_ARRAY_D, a)
    final def getArrayD = unsafe.getObject(this, OFFSET_ARRAY_D).asInstanceOf[Array[Iterable[T]]]
    final def getSubIter = unsafe.getObject(this, OFFSET_SUBITER).asInstanceOf[Iterator[T]]
    final def clearArrayStack() {
      val arrayStack = unsafe.getObject(this, OFFSET_ARRAY_STACK).asInstanceOf[Array[Array[Iterable[T]]]]
      var i = 0
      while (i < 6 && arrayStack(i) != null) {
        arrayStack(i) = null
        i += 1
      }
    }
    final def clearPosStack() {
      val posStack = unsafe.getObject(this, OFFSET_POS_STACK).asInstanceOf[Array[Int]]
      var i = 0
      while (i < 6) {
        posStack(i) = 0
        i += 1
      }
    }
    final def clearSubIter() = unsafe.putObject(this, OFFSET_SUBITER, null)
    final def root = getArrayD(0).asInstanceOf[Repr]
  }

  trait Scope {
    implicit def hashTrieSetOps[T](a: Par[HashSet[T]]) = new Trees.HashSetOps(a)
    implicit def canMergeHashTrieSet[T: ClassTag](implicit ctx: WorkstealingTreeScheduler) = new CanMergeFrom[Par[HashSet[_]], T, Par[HashSet[T]]] {
      def apply(from: Par[HashSet[_]]) = new HashSetMerger[T](ctx)
      def apply() = new HashSetMerger[T](ctx)
    }
    implicit def hashTrieSetIsReducable[T] = new IsReducable[HashSet[T], T] {
      def apply(pa: Par[HashSet[T]]) = ???
    }
    implicit def hashTrieMapOps[K, V](a: Par[HashMap[K, V]]) = new Trees.HashMapOps(a)
    implicit def canMergeHashTrieMap[K: ClassTag, V: ClassTag](implicit ctx: WorkstealingTreeScheduler) = new CanMergeFrom[Par[HashMap[_, _]], (K, V), Par[HashMap[K, V]]] {
      def apply(from: Par[HashMap[_, _]]) = new HashMapMerger[K, V](ctx)
      def apply() = new HashMapMerger[K, V](ctx)
    }
    implicit def hashTrieMapIsReducable[K, V] = new IsReducable[HashMap[K, V], (K, V)] {
      def apply(pa: Par[HashMap[K, V]]) = ???
    }
  }

  class HashSetOps[T](val hashset: Par[HashSet[T]]) extends AnyVal with Reducables.OpsLike[T, Par[HashSet[T]]] {
    def stealer: Stealer[T] = {
      val s = new HashSetStealer(hashset.seq)
      s.rootInit()
      s
    }
    override def aggregate[S](z: S)(combop: (S, S) => S)(seqop: (S, T) => S)(implicit ctx: WorkstealingTreeScheduler) = macro methods.HashTrieSetMacros.aggregate[T, S]
    override def mapReduce[M](mp: T => M)(combop: (M, M) => M)(implicit ctx: WorkstealingTreeScheduler) = macro methods.HashTrieSetMacros.mapReduce[T, T, M]
    override def min[U >: T](implicit ord: Ordering[U], ctx: WorkstealingTreeScheduler): U = macro methods.HashTrieSetMacros.min[T, U]
    override def max[U >: T](implicit ord: Ordering[U], ctx: WorkstealingTreeScheduler): U = macro methods.HashTrieSetMacros.max[T, U]
    override def reduce[U >: T](operator: (U, U) => U)(implicit ctx: WorkstealingTreeScheduler) = macro methods.HashTrieSetMacros.reduce[T, U]
    override def foreach[U >: T](action: U => Unit)(implicit ctx: WorkstealingTreeScheduler): Unit = macro methods.HashTrieSetMacros.foreach[T, U]
    override def fold[U >: T](z: => U)(op: (U, U) => U)(implicit ctx: WorkstealingTreeScheduler): U = macro methods.HashTrieSetMacros.fold[T, U]
    override def sum[U >: T](implicit num: Numeric[U], ctx: WorkstealingTreeScheduler): U = macro methods.HashTrieSetMacros.sum[T, U]
    override def product[U >: T](implicit num: Numeric[U], ctx: WorkstealingTreeScheduler): U = macro methods.HashTrieSetMacros.product[T, U]
    override def count[U >: T](p: U => Boolean)(implicit ctx: WorkstealingTreeScheduler): Int = macro methods.HashTrieSetMacros.count[T, U]
    override def find[U >: T](pred: U => Boolean)(implicit ctx: WorkstealingTreeScheduler): Option[T] = macro methods.HashTrieSetMacros.find[T, U]
    override def exists[U >: T](p: U => Boolean)(implicit ctx: WorkstealingTreeScheduler): Boolean = macro methods.HashTrieSetMacros.exists[T, U]
    override def forall[U >: T](p: U => Boolean)(implicit ctx: WorkstealingTreeScheduler): Boolean = macro methods.HashTrieSetMacros.forall[T, U]


    override def map[S, That](func: T => S)(implicit cmf: CanMergeFrom[Par[HashSet[T]], S, That], ctx: WorkstealingTreeScheduler): That = macro methods.HashTrieSetMacros.map[T, S, That]
    override def flatMap[S, That](func: T => TraversableOnce[S])(implicit cmf: CanMergeFrom[Par[HashSet[T]], S, That], ctx: WorkstealingTreeScheduler): That = macro methods.HashTrieSetMacros.flatMap[T, S, That]
    override def filter[That](pred: T => Boolean)(implicit cmf: CanMergeFrom[Par[HashSet[T]], T, That], ctx: WorkstealingTreeScheduler): That = macro methods.HashTrieSetMacros.filter[T, That]
    def seq = hashset
  }

  class HashMapOps[K, V](val hashmap: Par[HashMap[K, V]]) extends AnyVal with Reducables.OpsLike[(K, V), Par[HashMap[K, V]]] {
    def stealer: Stealer[(K, V)] = {
      val s = new HashMapStealer(hashmap.seq)
      s.rootInit()
      s
    }
    override def aggregate[S](z: S)(combop: (S, S) => S)(seqop: (S, (K, V)) => S)(implicit ctx: WorkstealingTreeScheduler) = macro methods.HashTrieMapMacros.aggregate[K, V, S]
    override def mapReduce[M](mp: ((K, V)) => M)(combop: (M, M) => M)(implicit ctx: WorkstealingTreeScheduler) = macro methods.HashTrieMapMacros.mapReduce[K, V, M]
    override def min[U >: (K, V)](implicit ord: Ordering[U], ctx: WorkstealingTreeScheduler): U = macro methods.HashTrieMapMacros.min[K, V, U]
    override def max[U >: (K, V)](implicit ord: Ordering[U], ctx: WorkstealingTreeScheduler): U = macro methods.HashTrieMapMacros.max[K, V, U]
    override def reduce[U >: (K, V)](operator: (U, U) => U)(implicit ctx: WorkstealingTreeScheduler) = macro methods.HashTrieMapMacros.reduce[K, V, U]
    override def foreach[U >: (K, V)](action: U => Unit)(implicit ctx: WorkstealingTreeScheduler): Unit = macro methods.HashTrieMapMacros.foreach[K, V, U]
    override def fold[U >: (K, V)](z: => U)(op: (U, U) => U)(implicit ctx: WorkstealingTreeScheduler): U = macro methods.HashTrieMapMacros.fold[K, V, U]
    override def sum[U >: (K, V)](implicit num: Numeric[U], ctx: WorkstealingTreeScheduler): U = macro methods.HashTrieMapMacros.sum[K, V, U]
    override def product[U >: (K, V)](implicit num: Numeric[U], ctx: WorkstealingTreeScheduler): U = macro methods.HashTrieMapMacros.product[K, V, U]
    override def count[U >: (K, V)](p: U => Boolean)(implicit ctx: WorkstealingTreeScheduler): Int = macro methods.HashTrieMapMacros.count[K, V, U]
    override def find[U >: (K, V)](pred: U => Boolean)(implicit ctx: WorkstealingTreeScheduler): Option[(K, V)] = macro methods.HashTrieMapMacros.find[K, V, U]
    override def exists[U >: (K, V)](p: U => Boolean)(implicit ctx: WorkstealingTreeScheduler): Boolean = macro methods.HashTrieMapMacros.exists[K, V, U]

    override def forall[U >: (K, V)](p: U => Boolean)(implicit ctx: WorkstealingTreeScheduler): Boolean = macro methods.HashTrieMapMacros.forall[K, V, U]


    override def map[S, That](func: ((K, V)) => S)(implicit cmf: CanMergeFrom[Par[HashMap[K, V]], S, That], ctx: WorkstealingTreeScheduler): That = macro methods.HashTrieMapMacros.map[K, V, S, That]
    override def flatMap[S, That](func: ((K, V)) => TraversableOnce[S])(implicit cmf: CanMergeFrom[Par[HashMap[K, V]], S, That], ctx: WorkstealingTreeScheduler): That = macro methods.HashTrieMapMacros.flatMap[K, V, S, That]
    override def filter[That](pred: ((K, V)) => Boolean)(implicit cmf: CanMergeFrom[Par[HashMap[K, V]], (K, V), That], ctx: WorkstealingTreeScheduler): That = macro methods.HashTrieMapMacros.filter[K, V, That]
    def seq = hashmap
  }

  class HashSetStealer[T](val root: HashSet[T]) extends {
    val classTag = implicitly[ClassTag[HashSet[T]]]
    val totalSize = root.size
  } with TreeStealer.External[T, HashSet[T]] {
    val chunkIterator = new TrieChunkIterator[T, HashSet[T]] {
      final def getElem(x: AnyRef): T = {
        val hs1 = x.asInstanceOf[HashSet.HashSet1[T]]
        Trees.key(hs1)
      }
    }
    val leafArray = new Array[Iterable[T]](1)

    var padding10: Int = 0
    var padding11: Int = 0
    var padding12: Int = 0
    var padding13: Int = 0
    var padding14: Int = 0
    var padding15: Int = 0

    final def newStealer = new HashSetStealer(root)
    final def resetIterator(n: HashSet[T]): Unit = if (n.nonEmpty) {
      chunkIterator.setDepth(0)
      chunkIterator.setPosD(0)
      chunkIterator.clearArrayStack()
      chunkIterator.clearPosStack()
      chunkIterator.clearSubIter()
      leafArray(0) = n
      chunkIterator.setArrayD(leafArray)
    } else {
      chunkIterator.clearSubIter()
      chunkIterator.setDepth(-1)
    }
    final def child(n: HashSet[T], idx: Int) = {
      val trie = n.asInstanceOf[HashSet.HashTrieSet[T]]
      if (trie.elems.length > 1 || idx == 1) trie.elems(idx - 1)
      else if (idx == 2) HashSet.empty
      else sys.error("error state")
    }
    final def elementAt(n: HashSet[T], idx: Int): T = ???
    final def depthBound(totalSize: Int): Int = 6
    final def isLeaf(n: HashSet[T]) = n match {
      case _: HashSet.HashTrieSet[_] => false
      case _ => true
    }
    final def estimateSubtree(n: HashSet[T], depth: Int, totalSize: Int) = n.size
    final def totalChildren(n: HashSet[T]) = n match {
      case n: HashSet.HashTrieSet[_] =>
        val len = n.elems.length
        if (len == 1) 2 else len
      case _ =>
        0
    }
  }

  trait HashUtils {
    protected final def improve(hcode: Int) = {
      var h: Int = hcode + ~(hcode << 9)
      h = h ^ (h >>> 14)
      h = h + (h << 4)
      h ^ (h >>> 10)
    }
  }

  class HashSetMerger[@specialized(Int, Long) T: ClassTag](
    val ctx: WorkstealingTreeScheduler) extends HashBuckets[T, T, HashSetMerger[T], Par[HashSet[T]]] with HashUtils {
    val elems = new Array[Conc.Buffer[T]](1 << width)

    def newHashBucket = new HashSetMerger[T](ctx)

    def width = 5

    def clearBucket(idx: Int) {
      elems(idx) = null
    }

    def mergeBucket(idx: Int, that: HashSetMerger[T], res: HashSetMerger[T]) {
      val thise = this.elems(idx)
      val thate = that.elems(idx)
      if (thise == null) {
        res.elems(idx) = thate
      } else if (thate == null) {
        res.elems(idx) = thise
      } else {
        thise.prepareForMerge()
        thate.prepareForMerge()
        res.elems(idx) = thise merge thate
      }
    }

    def +=(elem: T): HashSetMerger[T] = {
      val es = elems
      val hc = improve(elem.##)
      val idx = hc & 0x1f
      var bucket = es(idx)
      if (bucket eq null) {
        es(idx) = new Conc.Buffer[T]
        bucket = es(idx)
      }
      bucket += elem
      this
    }

    def result = {
      import Par._
      import Ops._
      val stealer = (0 until elems.length).toPar.stealer
      val root = new Array[HashSet[T]](1 << 5)
      val kernel = new HashSetMergerResultKernel(elems, root)

      ctx.invokeParallelOperation(stealer, kernel)

      val fulltries = root.filter(_ != null)
      var bitmap = 0
      var sz = 0
      var i = 0
      while (i < root.length) {
        if (root(i) ne null) {
          sz += root(i).size
          bitmap |= 1 << i
        }
        i += 1
      }

      val hs = if (sz == 0) HashSet.empty[T]
      else if (sz == 1) root(0)
      else new HashSet.HashTrieSet(bitmap, fulltries, sz)

      hs.toPar
    }
  }

  class HashSetMergerResultKernel[@specialized(Int, Long) T](
    val elems: Array[Conc.Buffer[T]],
    val root: Array[HashSet[T]]) extends IndexedStealer.IndexedKernel[Int, Unit] with HashUtils {
    override def incrementStepFactor(config: WorkstealingTreeScheduler.Config) = 1
    def zero = ()
    def combine(a: Unit, b: Unit) = a
    def apply(node: Node[Int, Unit], chunkSize: Int) {
      val stealer = node.stealer.asInstanceOf[Ranges.RangeStealer]
      var i = stealer.nextProgress
      val until = stealer.nextUntil
      while (i < until) {
        storeBucket(i)
        i += 1
      }
    }
    private def storeBucket(i: Int) {
      def traverse(es: Conc[T], res: HashSet[T]): HashSet[T] = es match {
        case knode: Conc.<>[T] =>
          val lres = traverse(knode.left, res)
          traverse(knode.right, lres)
        case kchunk: Conc.Chunk[T] =>
          var cres = res
          val kchunkarr = kchunk.elems
          var i = 0
          val until = kchunk.size
          while (i < until) {
            val k = kchunkarr(i)
            val hc = improve(k.##)
            cres = cres.updated0(k, hc, 5)
            i += 1
          }
          cres
        case _ =>
          sys.error("unreachable: " + es)
      }

      root(i) = if (elems(i) ne null) traverse(elems(i).result.normalized, HashSet.empty[T]) else null
    }
  }

  abstract class HashSetKernel[T, R] extends Kernel[T, R] {
    def apply(node: Node[T, R], chunkSize: Int): R = {
      val stealer = node.stealer.asInstanceOf[HashSetStealer[T]]
      apply(node, stealer.chunkIterator)
    }
    def apply(node: Node[T, R], ci: TrieChunkIterator[T, HashSet[T]]): R
  }

  class HashMapStealer[K, V](val root: HashMap[K, V]) extends {
    val classTag = implicitly[ClassTag[HashMap[K, V]]]
    val totalSize = root.size
  } with TreeStealer.External[(K, V), HashMap[K, V]] {
    val chunkIterator = new TrieChunkIterator[(K, V), HashMap[K, V]] {
      final def getElem(x: AnyRef): (K, V) = {
        val hm1 = x.asInstanceOf[HashMap.HashMap1[K, V]]
        Trees.kv(hm1)
      }
    }
    val leafArray = new Array[Iterable[(K, V)]](1)

    var padding10: Int = 0
    var padding11: Int = 0
    var padding12: Int = 0
    var padding13: Int = 0
    var padding14: Int = 0
    var padding15: Int = 0

    final def newStealer = new HashMapStealer(root)
    final def resetIterator(n: HashMap[K, V]): Unit = if (n.nonEmpty) {
      chunkIterator.setDepth(0)
      chunkIterator.setPosD(0)
      chunkIterator.clearArrayStack()
      chunkIterator.clearPosStack()
      chunkIterator.clearSubIter()
      leafArray(0) = n
      chunkIterator.setArrayD(leafArray)
    } else {
      chunkIterator.clearSubIter()
      chunkIterator.setDepth(-1)
    }
    final def child(n: HashMap[K, V], idx: Int) = {
      val trie = n.asInstanceOf[HashMap.HashTrieMap[K, V]]
      if (trie.elems.length > 1 || idx == 1) trie.elems(idx - 1)
      else if (idx == 2) HashMap.empty
      else sys.error("error state")
    }
    final def elementAt(n: HashMap[K, V], idx: Int): (K, V) = ???
    final def depthBound(totalSize: Int): Int = 6
    final def isLeaf(n: HashMap[K, V]) = n match {
      case _: HashMap.HashTrieMap[_, _] => false
      case _ => true
    }
    final def estimateSubtree(n: HashMap[K, V], depth: Int, totalSize: Int) = n.size
    final def totalChildren(n: HashMap[K, V]) = n match {
      case n: HashMap.HashTrieMap[_, _] =>
        val len = n.elems.length
        if (len == 1) 2 else len
      case _ =>
        0
    }
  }

  class HashMapMerger[@specialized(Int, Long) K: ClassTag, @specialized(Int, Long) V: ClassTag](
    val ctx: WorkstealingTreeScheduler) extends HashBuckets[K, (K, V), HashMapMerger[K, V], Par[HashMap[K, V]]] with HashUtils {
    val keys = new Array[Conc.Buffer[K]](1 << width)
    val vals = new Array[Conc.Buffer[V]](1 << width)

    def newHashBucket = new HashMapMerger[K, V](ctx)

    def width = 5

    def clearBucket(idx: Int) {
      keys(idx) = null
      vals(idx) = null
    }

    def mergeBucket(idx: Int, that: HashMapMerger[K, V], res: HashMapMerger[K, V]) {
      val thisks = this.keys(idx)
      val thatks = that.keys(idx)
      val thisvs = this.vals(idx)
      val thatvs = that.vals(idx)
      if (thisks == null) {
        res.keys(idx) = thatks
        res.vals(idx) = thatvs
      } else if (thatks == null) {
        res.keys(idx) = thisks
        res.vals(idx) = thisvs
      } else {
        thisks.prepareForMerge()
        thatks.prepareForMerge()
        res.keys(idx) = thisks merge thatks
        thisvs.prepareForMerge()
        thatvs.prepareForMerge()
        res.vals(idx) = thisvs merge thatvs
      }
    }

    def +=(kv: (K, V)): HashMapMerger[K, V] = {
      val ks = keys
      val vs = vals
      val hc = improve(kv._1.##)
      val idx = hc & 0x1f
      var kbucket = ks(idx)
      if (kbucket eq null) {
        ks(idx) = new Conc.Buffer[K]
        vs(idx) = new Conc.Buffer[V]
        kbucket = ks(idx)
      }
      val vbucket = vs(idx)
      kbucket += kv._1
      vbucket += kv._2
      this
    }

    def result = {
      import Par._
      import Ops._
      val stealer = (0 until keys.length).toPar.stealer
      val root = new Array[HashMap[K, V]](1 << 5)
      val kernel = new HashMapMergerResultKernel(keys, vals, root)

      ctx.invokeParallelOperation(stealer, kernel)

      val fulltries = root.filter(_ != null)
      var bitmap = 0
      var sz = 0
      var i = 0
      while (i < root.length) {
        if (root(i) ne null) {
          sz += root(i).size
          bitmap |= 1 << i
        }
        i += 1
      }

      val hm = if (sz == 0) HashMap.empty[K, V]
      else if (sz == 1) root(0)
      else new HashMap.HashTrieMap(bitmap, fulltries, sz)

      hm.toPar
    }
  }

  class HashMapMergerResultKernel[@specialized(Int, Long) K, @specialized(Int, Long) V](
    val keys: Array[Conc.Buffer[K]],
    val vals: Array[Conc.Buffer[V]],
    val root: Array[HashMap[K, V]]) extends IndexedStealer.IndexedKernel[Int, Unit] with HashUtils {
    override def incrementStepFactor(config: WorkstealingTreeScheduler.Config) = 1
    def zero = ()
    def combine(a: Unit, b: Unit) = a
    def apply(node: Node[Int, Unit], chunkSize: Int) {
      val stealer = node.stealer.asInstanceOf[Ranges.RangeStealer]
      var i = stealer.nextProgress
      val until = stealer.nextUntil
      while (i < until) {
        storeBucket(i)
        i += 1
      }
    }
    private def storeBucket(i: Int) {
      def traverse(keys: Conc[K], vals: Conc[V], res: HashMap[K, V]): HashMap[K, V] = keys match {
        case knode: Conc.<>[K] =>
          val vnode = vals.asInstanceOf[Conc.<>[V]]
          val lres = traverse(knode.left, vnode.left, res)
          traverse(knode.right, vnode.right, lres)
        case kchunk: Conc.Chunk[K] =>
          val vchunk = vals.asInstanceOf[Conc.Chunk[V]]
          var cres = res
          val kchunkarr = kchunk.elems
          val vchunkarr = vchunk.elems
          var i = 0
          val until = kchunk.size
          while (i < until) {
            val k = kchunkarr(i)
            val v = vchunkarr(i)
            val hc = improve(k.##)
            cres = cres.updated0(k, hc, 5, v, null, null)
            i += 1
          }
          cres
        case _ =>
          sys.error("unreachable: " + keys + ", " + vals)
      }

      root(i) = if (keys(i) ne null) traverse(keys(i).result.normalized, vals(i).result.normalized, HashMap.empty[K, V]) else null
    }
  }

  abstract class HashMapKernel[K, V, R] extends Kernel[(K, V), R] {
    def apply(node: Node[(K, V), R], chunkSize: Int): R = {
      val stealer = node.stealer.asInstanceOf[HashMapStealer[K, V]]
      apply(node, stealer.chunkIterator)
    }
    def apply(node: Node[(K, V), R], ci: TrieChunkIterator[(K, V), HashMap[K, V]]): R
  }

}