import io._
import expr._
import java.io._
import java.nio.file.{Paths, Files}
import java.util.Scanner
import jline.console.ConsoleReader

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
          // Otherwise, spin up the REPL
          println("Welcome to LittleScheme...")
          println("All ye who enter here beware")
          println("")

          val reader = new ConsoleReader();
          reader.setPrompt("> ");

          var line = ""
          val out = new PrintWriter(reader.getOutput());

          while ((line = reader.readLine()) != null) {
            out.println("Echo: " + line);
            out.flush();
          }
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
