import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers, PropSpec}

import scala.collection.BitSet
import scala.collection.immutable.{HashSet, TreeSet}

/**
  * Created by jdziworski on 03.08.16.
  */
class PropertiesSrcToEntriesTest extends PropSpec with TableDrivenPropertyChecks with Matchers {

  val correctProperties = Table(
    "correctProperties",
    ("protocol=http\nurl=192.16.55", Map("protocol" -> "http", "url" -> "192.16.55")),
    ("pr%%%13*88oto3//c%ol=//**http", Map("pr%%%13*88oto3//c%ol" -> "//**http"))
  )

  val incorrectProperties = Table(
    "incorrectProperties",
    "  "
  )

  property("correct properties should be converted to tuple") {
    forAll(correctProperties) { properties =>
      PropertiesGenerator.stringToEntries(properties._1) shouldEqual properties._2
    }
  }

  property("testPropertiesSrcToEntries") {
    forAll(incorrectProperties) { properties =>
      a[InvalidEntryException] should be thrownBy {
        PropertiesGenerator.stringToEntries(properties)
      }
    }
  }

}
