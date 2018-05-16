package com.swashbuckle.tutorial.part_4_constants

import java.io.File

import scala.meta._

object ConstantsValidator extends App {
  case class Val(valName: scala.meta.Pat, valValue: String)

  def validate(source: Source): Unit = source match {
    case source"..$stats" => stats.collect(_ match {
      case q"..$mods object ${Term.Name(name)} extends $template" => name match{
        case "Constants" => template match {
          case template"{ ..$stats2 } with ..$ctorcalls { $param => ..$stats3 }" =>{
            val vals: List[Val] = stats3.foldLeft(List[Val]()) {
              (acc, elem) => elem match {
                case q"..$mods2 val ..$patsnel: $tpeopt = $expr" => acc :+ Val(patsnel.head, expr.toString)
                case _ => acc
              }
            }
            vals.groupBy(_.valValue).foreach{ case
              (valueKey, listOfVals) => if (listOfVals.length > 1 ) throw new Exception(s"$valueKey is assigned more than once to different vals: ${listOfVals.map(_.valName)}")
            }
          }
        }
        case _ =>
      }
    })
  }

  validate(new File("src/main/scala/Constants.scala").parse[Source].get)
}
