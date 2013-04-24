package scala.collection.parallel
package workstealing



import scala.annotation.tailrec



abstract class WorkstealingTreeScheduler {
  import WorkstealingTreeScheduler._

  val invoker = new Worker {
    def index = 0
    def total = config.parallelismLevel
    def name = "Invoker"
  }

  def config: Config

  def dispatchWork[T, R](root: Ref[T, R], kernel: Kernel[T, R]): Unit

  def joinWork[T, R](root: Ref[T, R], kernel: Kernel[T, R]): Unit

  /*@tailrec*/ final def workUntilNoWork[T, R](w: Worker, root: Ref[T, R], kernel: Kernel[T, R]) {
    //val leaf = config.strategy.findWork[T, R](w, root, kernel).asInstanceOf[Ptr[T, R]]
    //if (leaf != null) {
    //  @tailrec def workAndDescend(leaf: Ptr[T, R]) {
    //    val nosteals = kernel.workOn(leaf, w)
    //    if (!nosteals) {
    //      val subnode = config.strategy.chooseAsVictim[T, R](w.index, w.total, leaf).asInstanceOf[Ptr[T, R]]
    //      if (subnode.child.tryOwn(w)) workAndDescend(subnode)
    //    }
    //  }
    //  workAndDescend(leaf)
    //  workUntilNoWork(w, root, kernel)
    //} else {
    //  // no more work
    //}
  }

  def invokeParallelOperation[T, R](kernel: Kernel[T, R], stealer: Stealer[T]): R = {
    // create workstealing tree
    val node = new Node[T, R](null, null)(stealer)
    val root = new Ref[T, R](null, 0)(node)
    val work = root.child
    kernel.afterCreateRoot(root)
    work.tryOwn(invoker)

    // let other workers know there's something to do
    dispatchWork(root, kernel)

    // piggy-back the caller into doing work
    //if (!kernel.workOn(root, Invoker)) workUntilNoWork(Invoker, root, kernel.repr)

    // synchronize in case there's some other worker just
    // about to complete work
    joinWork(root, kernel)

    val c = root.READ
    val r = c.READ_RESULT
    r.asInstanceOf[R]
  }


}


object WorkstealingTreeScheduler {

  val CHILD_OFFSET = unsafe.objectFieldOffset(classOf[Ref[_, _]].getDeclaredField("child"))
  val OWNER_OFFSET = unsafe.objectFieldOffset(classOf[Node[_, _]].getDeclaredField("owner"))
  val RESULT_OFFSET = unsafe.objectFieldOffset(classOf[Node[_, _]].getDeclaredField("result"))
  val NO_RESULT = new AnyRef
  val INTERMEDIATE_READY = new AnyRef

  trait Config {
    def parallelismLevel: Int
    def incrementStepFrequency: Int
    def maximumStep: Int
    def stealingStrategy: Strategy
  }

  trait Worker {
    def index: Int
    def total: Int
    def name: String
  }

  trait TerminationCause {
    def validateResult[R](r: R): R
  }

  trait Kernel[@specialized T, R] {
    /** Used for cancelling operations early (e.g. due to exceptions).
     *  Holds information on why the operation failed
     */
    @volatile protected var terminationCause: TerminationCause = null

    /** Returns `true` as long as `terminationCause` is `null`.
     */
    def notTerminated = terminationCause eq null

    /** Returns the result if there was no early termination.
     *  Otherwise may throw an exception based on the termination cause.
     */
    def validateResult(r: R): R = {
      if (notTerminated) r
      else terminationCause.validateResult(r)
    }

    /** Initializes the workstealing tree node.
     *
     *  By default does nothing, but some kernels may choose to override this default behaviour
     *  to store operation-specific information into the node.
     */
    def beforeWorkOn(tree: Ref[T, R], node: Node[T, R]) {
      node.WRITE_INTERMEDIATE(zero)
    }

    /** Initializes the workstealing tree root.
     *
     *  By default does nothing, but some kernels may choose to override this default behaviour.
     */
    def afterCreateRoot(tree: Ref[T, R]) {}

    /** Initializes a node that has just been expanded.
     * 
     *  By default does nothing, but some kernels may choose to override this default behaviour
     *  to store operation-specific information into the node.
     */
    def afterExpand(old: Node[T, R], node: Node[T, R]) {}
  
    /** Stores the result of processing the node into the `lresult` field.
     *
     *  This behaviour can be overridden.
     */
    def storeIntermediateResult(node: Node[T, R], res: R) {
      node.WRITE_INTERMEDIATE(res)
    }

    /** Returns true if completed with no stealing.
     *  Returns false if steal occurred.
     *
     *  May be overridden in subclass to specialize for better performance.
     */
    def workOn(tree: Ref[T, R], config: Config, worker: Worker): Boolean = {
      import Stealer._

      // atomically read the current node and initialize
      val node = tree.READ
      val stealer = node.stealer
      beforeWorkOn(tree, node)
      var intermediate = node.READ_INTERMEDIATE
      var incCount = 0
      val incFreq = config.incrementStepFrequency
      val ms = config.maximumStep

      // commit to processing chunks of the collection and process them until termination
      var looping = true
      while (looping && notTerminated) {
        val currstep = node.READ_STEP
        val currstate = stealer.state
  
        if (currstate != Completed && currstate != StolenOrExpanded) {
          // reserve some work
          val chunk = stealer.advance(currstep)

          if (chunk != -1) intermediate = combine(intermediate, apply(node, chunk))
  
          // update step
          incCount = (incCount + 1) % incFreq
          if (incCount == 0) node.WRITE_STEP(math.min(ms, currstep * 2))
        } else looping = false
      }

      completeIteration(node.stealer)

      // store into the `intermediateResult` field of the node and push result up
      completeNode(intermediate, tree, worker)
    }

    /** Completes the iteration in the stealer.
     * 
     *  Some parallel operations do not traverse all the elements in a chunk or a node. The purpose of this
     *  method is to bring the node into a Completed or Stolen state before proceeding.
     */
    protected final def completeIteration(stealer: Stealer[T]) {
      var state = stealer.state
      while (state eq Stealer.AvailableOrOwned) {
        stealer.markCompleted()
        state = stealer.state
      }
    }

    /** Completes the iteration in the node.
     * 
     *  Some parallel operations do not traverse all the elements in a chunk or a node. The purpose of this
     *  method is to bring the node into a Completed or Stolen state before proceeding.
     */
    private def completeNode(intermediate: R, tree: Ref[T, R], worker: Worker): Boolean = {
      import Stealer._
      val node_t0 = tree.READ
      val state_t1 = node_t0.stealer.state
      val wasCompleted = if (state_t1 eq Completed) {
        storeIntermediateResult(node_t0, intermediate)
        while (node_t0.READ_RESULT == NO_RESULT) node_t0.CAS_RESULT(NO_RESULT, INTERMEDIATE_READY)
        true
      } else if (state_t1 eq StolenOrExpanded) {
        // help expansion if necessary
        if (tree.READ.isLeaf) tree.markExpand(this, worker)

        val node_t2 = tree.READ
        storeIntermediateResult(node_t2, intermediate)
        while (node_t2.result == NO_RESULT) node_t2.CAS_RESULT(NO_RESULT, INTERMEDIATE_READY)
        false
      } else sys.error("unreachable: " + state_t1 + ", " + node_t0.toString(0))
  
      // push result up as far as possible
      pushUp(tree)
  
      wasCompleted
    }
    
    @tailrec private def pushUp(tree: Ref[T, R]) {
      val node_t0 = tree.READ
      val r_t1 = /*READ*/node_t0.result
      r_t1 match {
        case NO_RESULT =>
          // we're done, owner did not finish his work yet
        case INTERMEDIATE_READY =>
          if (node_t0.isLeaf) node_t0.READ_INTERMEDIATE
          else {
            // check if result already set for children
            def available(r: AnyRef) = r != NO_RESULT && r != INTERMEDIATE_READY
            val lr = node_t0.left.READ.READ_RESULT
            val rr = node_t0.right.READ.READ_RESULT
            if (available(lr) && available(rr)) {
              val subr = combine(lr.asInstanceOf[R], rr.asInstanceOf[R])
              val r = combine(node_t0.READ_INTERMEDIATE, subr).asInstanceOf[AnyRef]
              if (node_t0.CAS_RESULT(r_t1, r)) {
                // if at root, notify completion, otherwise go one level up
                if (tree.up == null) tree.synchronized {
                  tree.notifyAll()
                } else pushUp(tree.up)
              } else pushUp(tree) // retry
            }
          }
        case _ =>
          // we're done, somebody else already pushed up
      }
    }    

    /** The neutral element of the reduction.
     */
    def zero: R
  
    /** Combines results from two chunks into the aggregate result.
     */
    def combine(a: R, b: R): R
  
    /** Processes the specified chunk.
     */
    def apply(node: Node[T, R], chunkSize: Int): R
  }

  final class Ref[T, R](val up: Ref[T, R], val level: Int)(@volatile var child: Node[T, R]) {
    def CAS(ov: Node[T, R], nv: Node[T, R]) = unsafe.compareAndSwapObject(this, CHILD_OFFSET, ov, nv)
    def WRITE(nv: Node[T, R]) = unsafe.putObjectVolatile(this, CHILD_OFFSET, nv)
    def READ = unsafe.getObjectVolatile(this, CHILD_OFFSET).asInstanceOf[Node[T, R]]

    /** Try to mark the node as stolen, expand the node and return `true` if node was expanded.
     *  Return `false` if node was completed.
     */
    @tailrec def markExpand(kernel: Kernel[T, R], worker: Worker): Boolean = {
      import Stealer._
      val child_t0 = READ
      val stealer_t0 = child_t0.stealer
      if (!child_t0.isLeaf) true else { // already expanded
        // first set progress to -progress
        val state_t1 = stealer_t0.state
        if (state_t1 eq Completed) false // already completed
        else {
          if (state_t1 ne StolenOrExpanded) {
            if (stealer_t0.markStolen()) markExpand(kernel, worker) // marked stolen - now move on to node creation
            else markExpand(kernel, worker) // wasn't marked stolen and failed marking stolen - retry
          } else { // already marked stolen
            // node effectively immutable (except for `lresult`, `rresult` and `result`) - expand it
            val expanded = child_t0.newExpanded(this, worker)
            kernel.afterExpand(child_t0, expanded)
            if (CAS(child_t0, expanded)) true // try to replace with expansion
            else markExpand(kernel, worker) // failure (spurious or due to another expand) - retry
          }
        }
      }
    }

    def toString(l: Int): String = "\t" * level + "Ref" + level + " -> " + child.toString(l)

  }

  final class Node[@specialized T, R](val left: Ref[T, R], val right: Ref[T, R])(val stealer: Stealer[T]) {
    @volatile var step: Int = 1
    @volatile var owner: Worker = null
    @volatile var intermediateResult: R = _
    @volatile var result: AnyRef = NO_RESULT

    def CAS_OWNER(ov: Worker, nv: Worker) = unsafe.compareAndSwapObject(this, OWNER_OFFSET, ov, nv)
    def WRITE_OWNER(nv: Worker) = unsafe.putObjectVolatile(this, OWNER_OFFSET, nv)
    def READ_OWNER = unsafe.getObjectVolatile(this, OWNER_OFFSET).asInstanceOf[Worker]

    def CAS_RESULT(ov: AnyRef, nv: AnyRef) = unsafe.compareAndSwapObject(this, RESULT_OFFSET, ov, nv)
    def WRITE_RESULT(nv: AnyRef) = unsafe.putObjectVolatile(this, RESULT_OFFSET, nv)
    def READ_RESULT = unsafe.getObjectVolatile(this, RESULT_OFFSET).asInstanceOf[AnyRef]

    def READ_INTERMEDIATE = intermediateResult
    def WRITE_INTERMEDIATE(v: R) = intermediateResult = v

    def READ_STEP = step
    def WRITE_STEP(v: Int) = step = v

    final def isLeaf = left eq null

    @tailrec final def tryOwn(thiz: Worker): Boolean = {
      val currowner = READ_OWNER
      if (currowner eq thiz) true
      else if (currowner != null) false
      else if (CAS_OWNER(currowner, thiz)) true
      else tryOwn(thiz)
    }

    final def trySteal(parent: Ref[T, R], kernel: Kernel[T, R], worker: Worker): Boolean = {
      parent.markExpand(kernel, worker)
    }

    final def newExpanded(parent: Ref[T, R], worker: Worker): Node[T, R] = {
      val (lstealer, rstealer) = stealer.split

      val lnode = new Node[T, R](null, null)(lstealer)
      lnode.owner = this.READ_OWNER
      val lref = new Ref(parent, parent.level + 1)(lnode)

      val rnode = new Node[T, R](null, null)(rstealer)
      rnode.owner = if (this.READ_OWNER eq worker) null else worker
      val rref = new Ref(parent, parent.level + 1)(rnode)

      val nnode = new Node[T, R](lref, rref)(this.stealer)
      nnode.owner = this.READ_OWNER

      nnode
    }
  
    def isEligibleForWork(worker: Worker): Boolean = {
      import Stealer._
      stealer.state match {
        case Completed =>
          (owner == null) || ((owner eq worker) && result == null)
        case StolenOrExpanded =>
          true
        case AvailableOrOwned =>
          stealer.elementsRemainingEstimate > stealer.minimumStealThreshold || (owner == null) || (owner eq worker)
      }
    }

    final def toString(lev: Int): String = {
      val subtree = if (!isLeaf) {
        "\n" + left.toString(lev + 1) + right.toString(lev + 1)
      } else "\n"
      stealer.toString + subtree
    }
  }

  abstract class Strategy {
    /** Finds work in the tree for the given worker, which is one out of `total` workers.
     *  This search may include stealing work.
     */
    def findWork[T, R](worker: Worker, tree: Ref[T, R], kernel: Kernel[T, R]): Ref[T, R]

    /** Returns true if the worker labeled with `index` with a total of
     *  `total` workers should go left at level `level`.
     *  Returns false otherwise.
     */
    def choose[T, R](index: Int, total: Int, tree: Ref[T, R]): Boolean

    /** Which node the stealer takes at this level. */
    def chooseAsStealer[T, R](index: Int, total: Int, tree: Ref[T, R]): Ref[T, R]

    /** Which node the victim takes at this level. */
    def chooseAsVictim[T, R](index: Int, total: Int, tree: Ref[T, R]): Ref[T, R]
  }

  object FindMax extends Strategy {

    @tailrec def findWork[T, R](worker: Worker, tree: Ref[T, R], kernel: Kernel[T, R]) = {
      def search(current: Ref[T, R]): Ref[T, R] = {
        val node = current.READ
        if (node.isLeaf) {
          if (node.isEligibleForWork(worker)) current else null
        } else {
          val left = search(node.left)
          val right = search(node.right)
          if (left != null && right != null) {
            val leftwork = left.READ.stealer.elementsRemainingEstimate
            val rightwork = right.READ.stealer.elementsRemainingEstimate
            if (leftwork > rightwork) left else right
          } else if (left != null) left else right
        }
      }

      val max = search(tree)
      if (max != null) {
        val node = /*READ*/max.child
        if (node.tryOwn(worker)) max
        else if (node.trySteal(max, kernel, worker)) {
          val subnode = chooseAsStealer(worker.index, worker.total, max)
          if (subnode.child.tryOwn(worker)) subnode
          else findWork(worker, tree, kernel)
        } else findWork(worker, tree, kernel)
      } else null
    }

    def choose[T, R](index: Int, total: Int, tree: Ref[T, R]) = sys.error("never called")

    def chooseAsStealer[T, R](index: Int, total: Int, tree: Ref[T, R]) = tree.READ.right

    def chooseAsVictim[T, R](index: Int, total: Int, tree: Ref[T, R]) = tree.READ.left

  }

}
