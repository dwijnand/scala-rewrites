


package fix

import scala.collection.compat._
object ReplaceToSrc {
  List(1).to(Set)
  Set(1).to(List)

  def m1(xs: Set[Int]): List[Int] =
    xs.to(scala.collection.immutable.List)

  List[Int]() // unrelated matching brackets
}
