package scala.collection.parallel






class Par[Repr](val xs: Repr) extends AnyVal


object Par {

  implicit class ops[Repr](val xs: Repr) extends AnyVal {
    def toPar = new Par(xs)
  }

}
