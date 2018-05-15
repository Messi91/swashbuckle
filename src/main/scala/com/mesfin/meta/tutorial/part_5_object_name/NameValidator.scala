package com.mesfin.meta.tutorial.part_5_object_name

import java.io.{File, FileWriter}

import scala.meta._

object NameValidator extends App {
  def validateName(source: Source): Unit ={
    val fixedFile: Source = source match {
      case source"..$stats" => source"..${buildNewStatements(stats)}"
    }

    val fw = new FileWriter("src/main/scala/Constants.scala")
    fw.write(fixedFile.syntax)
    fw.close()
  }

  private def buildNewStatements(stats: scala.collection.immutable.Seq[Stat]): List[Stat] = {
    stats.foldLeft(List[Stat]())((acc, elem) => elem match {
      case q"..$mods object ${Term.Name(name)} extends $template" =>
        val isFirstLetterOfObjectLowercase = Character.isLowerCase(name.head)
        if(isFirstLetterOfObjectLowercase){
          val newName = name.head.toString.toUpperCase + name.tail
          val objectWithFixedName = q"..$mods object ${Term.Name(newName)} extends $template"
          acc :+ objectWithFixedName
        }else {
          acc :+ q"..$mods object ${Term.Name(name)} extends $template"
        }
      case whatever => acc :+ whatever
    })
  }

  validateName(new File("src/main/scala/Constants.scala").parse[Source].get)
}
