package edu.spbau.master.scala.course.task1

import java.io.File

import scala.io.Source
import scala.util.Try

/**
  * Created by Dmitriy Baidin.
  */
object Task1 extends App {
  /**
    * TODO Прочитайте содержимое данного файла.
    * В случае неудачи верните сообщение соответствующего исключения.
    */
  def readThisWorksheet(): String = Try {
    val sourcePath = "./src/main/scala-2.11"
    Source.fromFile(
      new File(sourcePath,
        getClass.getCanonicalName
          .replace('.', '/')
          .replace("$", "") + ".scala"))
      .getLines()
      .mkString("\n")
  }.recover {
    case e: Throwable ⇒ e.getMessage
  }.get

  println(readThisWorksheet())

}
