import org.scalatest.{FunSuite, Matchers, PropSpec}
import PropertiesGenerator.CountryStringExtensions

/**
  * Created by jdziworski on 11.08.16.
  */
class CountryStringExtensionsTest extends FunSuite with Matchers  {

  val cases = List(
    (("/home/cos/PL/CLBPL.img.properties","PL","CZ"),"/home/cos/CZ/CLBCZ.img.properties"),
    (("/home/cos/PL/CLBPL.img.properties","PL","UA"),"/home/cos/UA/CLBUA.img.properties"),
    (("/home/cos/PL/CLBPL.img.properties","PL","UA"),"/home/cos/UA/CLBUA.img.properties"),
    (("/home/PLcos/PL/CLBPL.img.properties","PL","UA"),"/home/PLcos/UA/CLBUA.img.properties")
  )

  test("paths should be properyl replaced") {
    val x = cases foreach  {
      case ((path,srcCountry,destCountry),expectedResult) => assert(path.replacePath(srcCountry,destCountry) == expectedResult)
    }
  }

  test("PL -> CZ property key") {
    assert("CLBPPL.IMG.PL.SOMEKEY".replaceCountryKey("PL","CZ") == "CLBPCZ.IMG.CZ.SOMEKEY")
  }

  test("PL -> HU property key") {
    assert("CLBPPL.IMG.PL.SOMEKEY".replaceCountryKey("PL","HU") == "CLBPHU.IMG.HU.SOMEKEY")
  }

  test("PL -> UA property key") {
    assert("CLBPPL.IMG.PL.SOMEKEY".replaceCountryKey("PL","UA") == "CLBPUA.IMG.UA.SOMEKEY")
  }

}
