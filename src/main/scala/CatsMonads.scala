object CatsMonads {
    import cats.Monad
    import cats.syntax.functor._
    import cats.syntax.flatMap._

    def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
        for {
            x <- a
            y <- b
        } yield x*x + y*y
    }

//    sumSquare(3, 4)

    import cats.Id

    val sum = sumSquare(3: Id[Int], 4: Id[Int])
//    println(sum)

    val a = Monad[Id].pure(3)
//    println(a)

    val b = Monad[Id].flatMap(a)(_+1)
//    println(b)

    val fored = for {
        x <- a
        y <- b
    } yield x + y
//    println(fored)



    // EITHER
    import cats.syntax.either._

//    def countPositive(nums: List[Int]) = {
//        nums.foldLeft(Right(0)) { (acc, num) =>
//            if(num > 0) {
//                acc.map(_ + 1)
//            } else {
//                Left("Negative< Stop!")
//            }
//        }
//    }

    def countPositive(nums: List[Int]) = {
        nums.foldLeft(0.asRight[String]) { (acc, num) =>
            if(num > 0) {
                acc.map(_ + 1)
            } else {
                Left("Negative, Stop!")
            }
        }
    }

    val countedInts = countPositive(List(1, 2, 3))
//    println(countedInts)

    val countedNegInts = countPositive(List(1, -2, 3))
//    println(countedNegInts)

    val catched = Either.catchOnly[NumberFormatException]("foo".toInt)
//    println(catched)

    val catchedNonFatal = Either.catchNonFatal(sys.error("Bad"))
//    println(catchedNonFatal)

    val fromTry = Either.fromTry(scala.util.Try("foo".toInt))
//    println(fromTry)

    val fromOption = Either.fromOption[String, Int](None,"Badness")
//    println(fromOption)

    val errorHandling = for {
        a <- 1.asRight[String]
        b <- 0.asRight[String]
        c <- if(b == 0) "DIV0".asLeft[Int] else (a / b).asRight[String]
    } yield c * 100
//    println(errorHandling)

    // Type class MonadError


    // EVAL

    import cats.Eval
    val now = Eval.now(math.random() + 1000) // eager, memorized
    val always = Eval.always(math.random() + 2000) // lazy, not memorized
    val later = Eval.later(math.random() + 3000) // lazy, memorized

    val greeting = Eval
        .always{ println("Step 1"); "Hello" }
        .map{ str => println("Step 2 "); s"$str world!"}

//    val greet = greeting.value
//    println(greet)

    //defer

    def factorialUnsafe(n: BigInt): BigInt = if(n == 1) n else n * factorialUnsafe(n - 1)

//    factorialUnsafe(50000)

    def factorialSafe(n: BigInt): Eval[BigInt] = {
        if(n == 1) {
            Eval.now(n)
        } else {
           Eval.defer(factorialSafe(n - 1).map(_ * n))
        }
    }

//    val veryBigInt = factorialSafe(50000).value
//    println(veryBigInt)

//    Writer
    import cats.data.Writer
    import cats.instances.vector._

    val writer = Writer(Vector("ABC", "CDE"), 123)
//    println(writer)

    import cats.syntax.writer._
    val told = Vector("a", "b", "c").tell
//    println(told.written)

    // Reader

    import cats.data.Reader

    final case class Cat(name: String, favoriteFood: String)

    val catName: Reader[Cat, String] = Reader(cat => cat.name)

    val garfield = Cat("Garfield", "lasagne")
//    println(catName.run(garfield))

    val greetCat: Reader[Cat, String] = catName.map(name => s"Hello ${name}")
    val feedCat: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
    val greetAndFeed: Reader[Cat, String] =
        for {
            greet <- greetCat
            feed <- feedCat
        } yield s"$greet. $feed"

//    println(greetAndFeed(garfield))


    // State

    import cats.data.State

    val intStringState = State[Int, String] { state =>
        (state, s"The state is $state")
    }

    val (state, result) = intStringState.run(10).value

    val justTheState = intStringState.runS(10).value

    val justTheResult = intStringState.runA(10).value


    val step1 = State[Int, String] { num =>
        val ans = num + 1
        (ans, s"Result of step1: $ans")
    }

    val step2 = State[Int, String] { num =>
        val ans = num * 2
        (ans, s"Result of step2: $ans")
    }

    val both = for {
        a <- step1
        b <- step2
    } yield (a, b)

    val (c, d) = both.run(20).value
    println(c, d)

 def main(args: Array[String]): Unit = {}
}
