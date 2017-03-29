import io._
import sexp._
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

          def appendProgramToEnv(p: Program, env: Env): Env = {
            p.defs match {
              case Nil => {
                env
              }
              case first :: rest => appendProgramToEnv(
                Program(rest, p.exp),
                env + (first.name -> new Box[SExp](Some(SFunc(first.params, first.body, env)))))
            }
          }

          def replLoop(line: String, replEnv: Env): Unit = {
            try {
              if(line.equals("exit")) {
                out.println()
                System.exit(0)
              }
              val sexp = parseSExp("(" + line + ")")
              val prog = parseProgram(sexp)
              val res = interpProgram(prog, replEnv)
              out.println(res)
              out.flush()
              replLoop(reader.readLine(), appendProgramToEnv(prog, replEnv))
            }
            catch {
              case e: Exception => {
                out.println(e)
                replLoop(reader.readLine(), replEnv)
              }
            }
          }

          replLoop(reader.readLine(), initialEnv)

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
