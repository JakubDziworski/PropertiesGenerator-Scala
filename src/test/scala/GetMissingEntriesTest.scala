import org.scalatest.FunSuite

/**
  * Created by jdziworski on 03.08.16.
  */
class GetMissingEntriesTest extends FunSuite {

  test("should return empty") {
    val plProperties= Map("CLBPL.IMG.PL.SIZE"->"1920")
    val huProperties= Map("CLBHU.IMG.HU.SIZE"->"320")
    val missingEntries = PropertiesGenerator.getMissingEntries(plProperties,huProperties,"PL","HU")
    assert(missingEntries isEmpty)
  }

  test("should return one missing entry") {
    val plProperties= Map("CLBPL.IMG.PL.SIZE"->"1920")
    val huProperties : Map[String,String]= Map.empty
    val missingEntries = PropertiesGenerator.getMissingEntries(plProperties,huProperties,"PL","HU")
    assert(missingEntries == "CLBHU.IMG.HU.SIZE=1920")
  }

  test("should return one missing entry when one overlapped and one new") {
    val plProperties= Map("CLBPL.IMG.PL.SIZE"->"1920","CLBPL.IMG.PL.WIDTH" -> "1080")
    val huProperties= Map("CLBHU.IMG.HU.SIZE"->"320")
    val missingEntries = PropertiesGenerator.getMissingEntries(plProperties,huProperties,"PL","HU")
    assert(missingEntries == "\nCLBHU.IMG.HU.WIDTH=1080")
  }
}
