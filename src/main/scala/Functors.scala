object Functors {
    // anything with map method
    // List, Option, Either, Future

    import cats.instances.function._ // for Functor
    import cats.syntax.functor._ // for map
    val func1: Int => Double =
        (x: Int) => x.toDouble
    val func2: Double => Double =
        (y: Double) => y * 2
    (func1 map func2)(1) // composition using map
    // res3: Double = 2.0 // composition using map
    (func1 andThen func2)(1) // composition using andThen
    // res4: Double = 2.0 // composition using andThen
    func2(func1(1)) // composition written out by hand
    // res5: Double = 2.0


    // FUNCTOR is a type F[A]:
    // map (A => B) => F[B].

    import cats.Functor
    import cats.instances.list._ // for Functor
    import cats.instances.option._ // for Functor

    val list1 = List(1, 2, 3)
    // list1: List[Int] = List(1, 2, 3)
    val list2 = Functor[List].map(list1)(_ * 2)
    // list2: List[Int] = List(2, 4, 6)
    val option1 = Option(123)
    // option1: Option[Int] = Some(123)
    val option2 = Functor[Option].map(option1)(_.toString)
    // option2: Option[String] = Some("123")

    val func = (x: Int) => x + 1
    val liftedFunc = Functor[Option].lift(func)

    liftedFunc(Option(1)) // Some(2)


    Functor[List].as(list1, "As") //List("As", "As", "As")

    import cats.instances.function._ // for Functor
    import cats.syntax.functor._ // for map


    //  val func1 = (a: Int) => a + 1
    //  val func2 = (a: Int) => a * 2
    val func3 = (a: Double) => s"${a}!"
    val func4 = func1.map(func2).map(func3)

    func4(123)


    def doMath[F[_]](start: F[Int])
                    (implicit functor: Functor[F]): F[Int] =
        start.map(n => n + 1 * 2)

    import cats.instances.option._ // for Functor
    import cats.instances.list._ // for Functor
    doMath(Option(20))
    // res4: Option[Int] = Some(22)
    doMath(List(1, 2, 3))
    // res5: List[Int] = List(3, 4, 5)


    // custom type
    implicit val optionFunctor: Functor[Option] =
        new Functor[Option] {
            def map[A, B](value: Option[A])(func: A => B): Option[B] =
                value.map(func)
        }

    val opt = Some(List(1, 2, 3))
    Functor[Option].map(opt)(_.toString())


    def main(args: Array[String]): Unit = {
    }
}
