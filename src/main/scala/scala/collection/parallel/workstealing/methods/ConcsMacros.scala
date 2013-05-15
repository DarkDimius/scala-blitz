package scala.collection.parallel.workstealing.methods



import scala.language.experimental.macros
import scala.reflect.macros._
import scala.collection.parallel.generic._
import collection.parallel.Par
import collection.parallel.workstealing._
import collection.parallel.Configuration
import scala.collection.parallel.Conc
import scala.math



object ConcsMacros {

  /* macro implementations */

  def reduce[T: c.WeakTypeTag, U >: T: c.WeakTypeTag](c: Context)(operator: c.Expr[(U, U) => U])(ctx: c.Expr[WorkstealingTreeScheduler]): c.Expr[U] = {
    import c.universe._

    val (lv, op) = c.functionExpr2Local[(U, U) => U](operator)
    val calleeExpression = c.Expr[Concs.Ops[T]](c.applyPrefix)
    val result = reify {
      import scala.collection.parallel.workstealing._
      lv.splice
      val callee = calleeExpression.splice
      val stealer = callee.stealer
      val kernel = new scala.collection.parallel.workstealing.Concs.ConcKernel[T, ResultCell[U]] {
        override def beforeWorkOn(tree: WorkstealingTreeScheduler.Ref[T, ResultCell[U]], node: WorkstealingTreeScheduler.Node[T, ResultCell[U]]) {
          node.WRITE_INTERMEDIATE(new ResultCell[U])
        }
        def zero = new ResultCell[U]
        def combine(a: ResultCell[U], b: ResultCell[U]) = {
          if (a eq b) a
          else if (a.isEmpty) b
          else if (b.isEmpty) a
          else {
            val r = new ResultCell[U]
            r.result = op.splice(a.result, b.result)
            r
          }
        }
        final def applyTree(t: Conc[T], remaining: Int, cell: ResultCell[U]) = {
          def apply(t: Conc[T], remaining: Int): U = t match {
            case _: Conc.<>[T] | _: Conc.Append[T] =>
              val l = apply(t.left, remaining)
              val r = apply(t.right, remaining - t.left.size)
              op.splice(l, r)
            case c: Conc.Single[T] =>
              c.elem
            case c: Conc.Chunk[T] =>
              applyChunk(c, 0, remaining)
            case _ =>
              ???
          }

          val sum = apply(t, remaining)
          cell.result = if (cell.isEmpty) sum else op.splice(cell.result, sum)
          cell
        }
        final def applyChunk(c: Conc.Chunk[T], from: Int, remaining: Int, cell: ResultCell[U]) = {
          if (remaining > 0 && c.size > from) {
            val sum = applyChunk(c, from, remaining)
            cell.result = if (cell.isEmpty) sum else op.splice(cell.result, sum)
          }
          cell
        }
        private def min(a: Int, b: Int) = if (a < b) a else b
        private def applyChunk(c: Conc.Chunk[T], from: Int, remaining: Int): U = {
          var i = from + 1
          val until = min(from + remaining, c.size)
          var a = c.elems
          var sum: U = a(from)
          while (i < until) {
            sum = op.splice(sum, a(i))
            i += 1
          }
          sum
        }
      }
      val result = ctx.splice.invokeParallelOperation(stealer, kernel)
      result
    }

    val operation = reify {
      val res = result.splice
      if (res.isEmpty) throw new java.lang.UnsupportedOperationException("empty.reduce")
      else res.result
    }

    c.inlineAndReset(operation)
  }

}