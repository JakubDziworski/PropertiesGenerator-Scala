import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

/**
  * Created by jdziworski on 03.08.16.
  */
class ParserTest extends FlatSpec with Matchers {

  "Properties generator" should "parse args" in {
    val conf = PropertiesGenerator.parseArgs("-c PL -p CLBPL.IMG.properties".split(" ").toList)
    conf should equal(Config("PL","CLBPL.IMG.properties"))
  }

  it should "parse args in reverse order" in {
    val conf = PropertiesGenerator.parseArgs("-p CLBPL.IMG.properties -c PL".split(" ").toList)
    conf should equal(Config("PL","CLBPL.IMG.properties"))
  }

  it should "throw exception when onyl country provided"  in {
    val argsList = "-c PL".split(" ").toList
    intercept[IllegalParametersException] {
      PropertiesGenerator.parseArgs(argsList)
    }
  }

  it should "throw exception when only properties name provided"  in {
    val argsList = "-p CLBPL.IMG.properties".split(" ").toList
    intercept[IllegalParametersException] {
      PropertiesGenerator.parseArgs(argsList)
    }
  }

}
