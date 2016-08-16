import java.io.File
import java.nio.file.{Files, Paths}
import org.apache.commons.io.FileUtils
import scala.io.Source

/**
  * Created by jdziworski on 03.08.16.
  */

case class IllegalParametersException(args: List[String]) extends RuntimeException(s"command arguments ($args) are illegal.")

case class PropertiesFileNotFoundException(filePath: String) extends RuntimeException(s"property file $filePath not found")

case class UnknownCountryException(srcCountry: String) extends RuntimeException(s"country $srcCountry is not known")

case class InvalidEntryException(entryString: String) extends RuntimeException(s"Entry ($entryString) has bad format (no '=' sign)")

case class InvalidPropertiesFileNameException(path: String) extends RuntimeException(s"properties file path $path is not properly formatted")

case class Config(srcCountry: String, propertiesFilePath: String)

object PropertiesGenerator {

  type Entries = Map[String, String]

  val knownCountries = List("PL", "CZ", "UA", "RO", "SK", "HU")

  def main(args: Array[String]): Unit = {
    val config: Config = parseArgs(args.toList)
    validateConfig(config)
    processProperties(config)
  }

  def parseArgs(argsList: List[String]): Config = argsList match {
    case "-c" :: country :: "-p" :: propertiesFileName :: tail => Config(country, propertiesFileName)
    case "-p" :: propertiesFileName :: "-c" :: country :: tail => Config(country, propertiesFileName)
    case _ => throw IllegalParametersException(argsList)
  }

  def validateConfig(config: Config): Unit = {
    if (!Files.exists(Paths.get(config.propertiesFilePath))) {
      throw PropertiesFileNotFoundException(config.propertiesFilePath)
    }
    if (!knownCountries.contains(config.srcCountry)) {
      throw UnknownCountryException(config.srcCountry)
    }
  }

  def processProperties(config: Config) = {
    val srcFileString = Source.fromFile(config.propertiesFilePath).mkString
    val srcEntries = stringToEntries(srcFileString)
    for (destCountry <- knownCountries) {
      val destPropertiesFilePath = config.propertiesFilePath.replacePath(config.srcCountry, destCountry)
      val destEntries: Entries = Files.exists(Paths.get(destPropertiesFilePath)) match {
        case true => stringToEntries(Source.fromFile(destPropertiesFilePath).mkString)
        case false => Map()
      }
      val missingEntriesString = getMissingEntries(srcEntries, destEntries, config.srcCountry, destCountry)
      FileUtils.writeStringToFile(new File(destPropertiesFilePath), missingEntriesString, true)
    }
  }

  def getMissingEntries(srcEntries: Entries, destEntries: Entries, srcCountry: String, destCountry: String): String = {
    srcEntries
      .map { case (key, value) => (key.replaceCountryKey(srcCountry, destCountry), value) }
      .filter { case (key, value) => !destEntries.contains(key) }
      .map { case (key, value) => key + "=" + value }
      .map { props => if(destEntries.nonEmpty) {"\n" + props} else props }
      .mkString("\n")
  }

  def stringToEntries(propertiesString: String): Entries = {
    (for (srcLine <- propertiesString.lines.toList) yield {
      srcLine.split("=", 2) match {
        case Array(key, value) => (key, value)
        case _ => throw InvalidEntryException(srcLine)
      }
    }).toMap
  }

  implicit class CountryStringExtensions(val value: String) extends AnyVal {
    implicit def replaceCountryKey(from: String, to: String): String = {
      value.replaceAll(from + ".", to + ".")
    }

    implicit def replacePath(srcCountry: String, destCountry: String): String = {
      val pattern = s"(.*)/${srcCountry}/CLB${srcCountry}(.*)".r
      value match {
        case pattern(prefixPath, fileSuffix) => s"$prefixPath/${destCountry}/CLB${destCountry}$fileSuffix"
        case _ => throw InvalidPropertiesFileNameException(value)
      }
    }
  }

}

