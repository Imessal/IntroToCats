object Monads {
//    A monad is a mechanism for sequencing computations.

    def parseInt(str: String): Option[Int] = scala.util.Try(str.toInt).toOption
    def divide(a: Int, b: Int): Option[Int]= if(b == 0) None else Some(a / b)

    def stringDivideBy(aStr: String, bStr: String): Option[Int] =
        parseInt(aStr).flatMap {
            aNum => parseInt(bStr).flatMap { bNum =>
                divide(aNum, bNum)
            }
        }

//    println(stringDivideBy("6", "2"))
//    println(stringDivideBy("6", "0"))
//    println(stringDivideBy("6", "a"))
//    println(stringDivideBy("b", "2"))

    // for-comprehensions
    def forStringDivideBy(aStr: String, bStr: String): Option[Int] =
        for {
            aNum <- parseInt(aStr)
            bNum <- parseInt(bStr)
            ans <- divide(aNum, bNum)
        } yield ans

//    println(forStringDivideBy("6", "2"))
//    println(forStringDivideBy("6", "0"))
//    println(forStringDivideBy("6", "a"))
//    println(forStringDivideBy("b", "2"))


//    MONAD :
//        1) pure, A => F[A]
//        2) flatMap, (F[A], A => F[B]) => F[B]
//    flatMap:
//        1) Left Identity: pure(a).flatMap(func) == func(a)
//        2) Right Identity: m.flatMap(pure) == m
//        3) Associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

    import cats.Monad
    import cats.instances.option._
    import cats.instances.list._

    val opt1 = Monad[Option].pure(3) // Some(3)
    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2)) // Some(5)
    val opt3 = Monad[Option].map(opt2)(a => 100 * a) // Some(500)

    val list1 = Monad[List].pure(3) // List(3)

    val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a*10))
//    println(list2)

    val list3 = Monad[List].map(list2)(a => a + 123)
//    println(list3)


//    MONAD SYNTAX
    import cats.syntax.applicative._

    1.pure[Option] //Some(1)
    1.pure[List] // List(1)

    import cats.syntax.flatMap._
    import cats.syntax.functor._ //for map

    def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
        a.flatMap(x => b.map(y => x*x + y*y))
    }

    val optSquare = sumSquare(Option(3), Option(4))
//    println(optSquare)

    val listSquare = sumSquare(List(1, 2, 3), List(4, 5))
//    println(listSquare)

    def sumSquareFor[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
        for {
            x <- a
            y <- b
        } yield x*x + y*y
    }

    def main(args: Array[String]): Unit = { }
}
