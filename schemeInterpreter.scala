import io._
import expr._
import java.io._
import java.nio.file.{Paths, Files}
import java.util.Scanner

object schemeInterpreter {
  def main(args: Array[String]): Unit = {
    try {
      var text = ""
      if (args.length == 0) {
        if (System.in.available() > 0) {
          // Check for input from System.in
          val sc = new Scanner(System.in)
          if (sc.hasNextLine()) {
            var next = ""
            while (sc.hasNextLine()) {
              next = sc.nextLine()
              text += next
            }
          }
        }
        else {
          // Otherwise, print an error
          System.err.println("USAGE: scheme <FILE or STRING>")
          System.exit(1)
        }
      }
      else if (Files.exists(Paths.get(args(0)))) {
        text = Source.fromFile(args(0)).mkString
      }
      else {
        text = args.mkString(" ")
      }
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
