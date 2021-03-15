import cats.Show
import cats.instances.int._
import cats.instances.string._

object HelloCats {
    val showInt = Show.apply[Int]
    val showString = Show.apply[String]

    val intAsString = showInt.show(42)
    val stringAsString = showString.show("Hello Cats!")

    //    val smthAsString = showInt.show("AAA")

    import cats.syntax.show._

    // Interface Syntax
    val shownInt = 123.show
    val shownString = "Hello Cats!".show



    case class Meal(soup: String, steak: String)

    implicit val mealShow: Show[Meal] = new Show[Meal] {
        def show(t: Meal): String = s"${t.soup} and ${t.steak} are great together!"
    }

    val lunch = Meal("Borscht", "Beef").show

    // Show's companion object methods
//        implicit val mealCompanionShow: Show[Meal] = Show.show(meal => s"${meal.soup} and ${meal.steak} are great together!")

    // Eq

    List(1, 2, 3).map(Some(_)).filter(i => i == 1)

    // тк мы импортировали cats.instances.int._
    import cats.Eq

    val eqInt = Eq[Int]

    eqInt.eqv(42, 42) // true
    eqInt.eqv(42, 43) // false
//        eqInt.eqv(42, "42")

    import cats.syntax.eq._

    42 === 42
    //    42 === "42"

    import cats.instances.option._

    (Some(1): Option[Int]) === (None: Option[Int]) //false

    import cats.syntax.option._
    1.some === none[Int] //false
    1.some =!= none[Int] //true

    // Comparing custom types
    import java.util.Date
    import cats.instances.long._

    implicit val dateEq: Eq[Date] = Eq.instance[Date] { (d1, d2) => d1.getTime === d2.getTime }

    val x = new Date()
    val y = new Date()

    x === x // true
    x === y // false

    def main(args: Array[String]): Unit = {
    }
}
