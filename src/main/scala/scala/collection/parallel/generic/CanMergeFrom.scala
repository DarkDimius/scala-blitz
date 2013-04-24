package scala.collection.parallel
package generic



import scala.annotation.implicitNotFound



@implicitNotFound(msg = "Cannot construct a parallel collection of type ${To} with elements of type ${Elem} based on a collection of type ${From}.")
trait CanMergeFrom[-From, -Elem, +To] {
  def apply(from: From): Merger[Elem, To]
  def apply(): Merger[Elem, To]
}
