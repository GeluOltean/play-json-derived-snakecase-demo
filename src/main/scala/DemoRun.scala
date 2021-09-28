import julienrf.json.derived
import julienrf.json.derived.{NameAdapter, TypeTag, TypeTagSetting}
import play.api.libs.json.JsonNaming.SnakeCase
import play.api.libs.json.{Json, OFormat}
import shapeless.Witness
import shapeless.labelled.FieldType

sealed trait DemoClass
final case class ClassOne(fieldOne: String, fieldTwo: String) extends DemoClass
final case class ClassTwo(nested: DemoClass, fieldOne: String, fieldTwo: String) extends DemoClass

object Values {
  val classOne: DemoClass = ClassOne("field one", "field two")
  val classTwo: DemoClass = ClassTwo(classOne, "field one", "field two")
}
object CurrentConversions {
  implicit val demoClassFormat: OFormat[DemoClass] = derived
    .withTypeTag
    .oformat[DemoClass](TypeTagSetting.FullClassName, NameAdapter.snakeCase)
}

object SnakeTagsConversions {
  trait SnakeCaseName[A] extends TypeTag[A]

  object SnakeCaseName {
    implicit def fromWitness[K <: Symbol, A](implicit wt: Witness.Aux[K]): SnakeCaseName[FieldType[K, A]] =
      new SnakeCaseName[FieldType[K, A]] {
        def value: String = SnakeCase.apply(wt.value.name)
      }
  }

  object SnakeCaseNameSetting extends TypeTagSetting {
    type Value[A] = SnakeCaseName[A]
  }

  implicit val demoClassFormat: OFormat[DemoClass] = derived
    .withTypeTag
    .oformat[DemoClass](SnakeCaseNameSetting, NameAdapter.snakeCase)
}


object RunIt extends App {
  def snakeTagsCasing(): Unit = {
    import SnakeTagsConversions._
    import Values._
    println(s"snake_case conversion: ${Json.toJson(classTwo)}")
  }

  def currentSnakeCasing(): Unit = {
    import CurrentConversions._
    import Values._
    println(s"Current conversion: ${Json.toJson(classTwo)}")
  }

  currentSnakeCasing()
  snakeTagsCasing()
}
