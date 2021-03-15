object HelloTypeClass {
    sealed trait Json
    final case class JsObject(get: Map[String, Json]) extends Json
    final case class JsString(get: String) extends Json
    final case class JsNumber(get: Double) extends Json
    final case object JsNull extends Json

    // Type Class
    trait JsonWriter[A] {
        def write(value: A): Json
    }

    final case class Person(name: String, surname: String)

    object JsonWriterInstances {
        // Type Class Instances == Implicit Values
        implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
            def write(value: String): Json = JsString(value)
        }

        implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person]{
            def write(value: Person): Json = JsObject(
                Map(
                    "name" -> JsString(value.name),
                    "surname" -> JsString(value.surname)
                )
            )
        }
    }

    object Json {
        def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
    }


    import JsonWriterInstances._

    private val person = Person("John", "Doe")

    private val jsoned = Json.toJson(person)
    private val jsonedWithImplicit = Json.toJson(person)(personWriter)

    println(jsoned == jsonedWithImplicit)

    // Interface Syntax
    object JsonSyntax {
        implicit class JsonWriterOps[A](value: A) {
            def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
        }
    }

    import JsonSyntax._
    //Extension Method
    person.toJson == person.toJson(personWriter)

    def main(args: Array[String]): Unit = {
    }
}
