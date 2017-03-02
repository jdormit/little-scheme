import io._
import expr._
import java.io._

object schemeInterpreter {
  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      System.err.println("USAGE: scheme FILE")
      System.exit(1)
    }
    try {
      val text = Source.fromFile(args(0)).mkString
      val result = evalProgram(text)
      println(result)
    }
    catch {
      case ioe: IOException => {
        System.err.println("Error reading file " + args(0))
        System.exit(1)
      }
      case e: Exception => {
        System.err.println(e.getMessage)
        System.exit(1)
      }
    }
  }
}
