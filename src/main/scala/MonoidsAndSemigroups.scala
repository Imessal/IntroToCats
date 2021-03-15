object MonoidsAndSemigroups {

    // MONOID:
    //  combine (A, A) => A
    //  empty element A
    //  associativeLaw
    //  identityLaw

    //  SEMIGROUP:
    //  MONOID / empty element

    import cats.Monoid
    import cats.Semigroup

    import cats.instances.string._
    import cats.instances.int._

    Monoid[String].combine("Hello ", "World")
    Monoid[String].empty
    Semigroup[String].combine("Hello ", "World")

    Monoid[Int].combine(32, 10) == Monoid.apply[Int].combine(32, 10)

    import cats.instances.option._

    val a = Option(30)
    val b = Option(12)

    val c = Monoid[Option[Int]].combine(a, b)

    // for |+|
    import cats.syntax.semigroup._

    val stringResult = "Hello " |+| "there!" |+| Monoid[String].empty

    import cats.instances.map._

    val map1 = Map("a" -> 1, "b" -> 2)
    val map2 = Map("b" -> 3, "d" -> 4)

    println(map1 |+| map2)


    def addAll[A](values: List[A])
                 (implicit monoid: Monoid[A]): A =
        values.foldRight(monoid.empty)(_ |+| _)

    addAll(List(1, 2, 3))
    addAll(List(None, Some(1), Some(2)))

    def main(args: Array[String]): Unit = {
        println(stringResult)
    }
}
