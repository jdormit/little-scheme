import sexp._

package object expr {
  class Box[A](var contents : Option[A] = None)

  type Env = Map[String, Box[SExp]]
  case class SFunc(params: List[String], body: Exp, env: Env) extends SExp
  case class Primitive(operation: String) extends SExp

  sealed abstract class Exp
  case class Literal(v: Int) extends Exp
  case class Ref(v: String) extends Exp
  case class Call(name: Exp, args: List[Exp]) extends Exp
  case class If(condition: Exp, lhs: Exp, rhs: Exp) extends Exp
  case class Quote(exp: SExp) extends Exp
  case class Lambda(params: List[String], body: Exp) extends Exp
  case class Print(exp: Exp) extends Exp

  case class Def(name: String, params: List[String], body: Exp)
  case class Program(defs: List[Def], exp: Exp)

  def parseExp(e: SExp) : Exp =
    e match {
      case SInt(v) => Literal(v)
      case STrue() => Quote(STrue())
      case SFalse() => Quote(SFalse())
      case SSymbol("null") => Quote(SNil)
      case SList(SSymbol("if"), cond, l, r) => If(parseExp(cond), parseExp(l), parseExp(r))
      case SList(SSymbol("and"), l, r) => If(parseExp(l), parseExp(r), Quote(SFalse()))
      case SSymbol(id) => Ref(id)
      case SList(
        SSymbol("let"),
        defs,
        body) => parseLet(defs, body, List(), List())
      case SList(
        SSymbol("let*"),
        defs,
        body) => parseLetStar(defs, body)
      case SList(SSymbol("quote"), exp) => Quote(exp)
      case SList(SSymbol("print"), exp) => Print(parseExp(exp))
      case SList(SSymbol("lambda"), params, body) => parseLambda(params, body, List())
      case SCons(id, args) => parseCall(args, parseExp(id), List())
      case _ => throw new IllegalArgumentException("Not a valid arithmetic expression: " + e)
    }

  // TODO the following three functions could be abstracted to a higher order function

  def parseLambda(params: SExp, body: SExp, acc: List[String]): Lambda =
    params match {
      case SNil => Lambda(acc.reverse, parseExp(body))
      case SCons(SSymbol(param), rest) => parseLambda(rest, body, param :: acc)
    }

  def parseCall(args: SExp, id: Exp, acc: List[Exp]) : Call =
    args match {
      case SNil => Call(id, acc.reverse)
      case SCons(first, rest) => parseCall(rest, id, parseExp(first) :: acc)
    }

  def parseLet(defs: SExp, body: SExp, ids: List[String], vals: List[Exp]) : Call =
    defs match {
      case SNil => Call(Lambda(ids, parseExp(body)), vals)
      case SCons(first, rest) => first match {
        case SList(SSymbol(id), exp) => parseLet(rest, body, id :: ids, parseExp(exp) :: vals)
      }
    }

  def parseLetStar(defs: SExp, body: SExp) : Call =
    defs match {
      case SNil => Call(Lambda(List(), parseExp(body)), List())
      case SCons(first, rest) => first match {
        case SList(SSymbol(id), exp) =>
          Call(Lambda(List(id), parseLetStar(rest, body)), List(parseExp(exp)))
      }
    }

  def parseDefine(name: String, params: SExp, body: SExp, acc: List[String]) : Def =
    params match {
      case SNil => Def(name, acc.reverse, parseExp(body))
      case SCons(SSymbol(id), rest) => parseDefine(name, rest, body, id :: acc)
    }

  def parseProgram(e: SExp) : Program = parseProgramHelper(e, List())

  def parseProgramHelper(e: SExp, defs: List[Def]) : Program = 
    e match {
      case SCons(first, rest) => first match {
        case SList(
          SSymbol("define"),
          SCons(SSymbol(name), params),
          body
        ) => parseProgramHelper(rest, parseDefine(name, params, body, List()) :: defs)
        case SList(
          SSymbol("define"),
          SList(SSymbol(name)),
          body
        ) => parseProgramHelper(rest, parseDefine(name, SNil, body, List()) :: defs)
        case exp: SExp => Program(defs.reverse, parseExp(exp))
        case _ => throw new IllegalArgumentException("Not a valid program: " + e)
      }
      // The program's expression could be empty
      case SNil => Program(defs.reverse, Quote(SNil))
      case _ => throw new IllegalArgumentException("Not a valid program: " + e)
    }

  def interpExp(e: Exp, env: Env) : SExp =
    e match {
      case Literal(v) => SInt(v)
      case Quote(sexp) => sexp
      case If(condition, l, r) => interpExp(condition, env) match {
        case STrue() => interpExp(l, env)
        case SFalse() => interpExp(r, env)
        case _ => interpExp(l, env)
      }
      case Ref(id) => env.get(id) match {
        case None => throw new RuntimeException("Unbound variable " + id)
        case Some(v) => v.contents match {
          case None => throw new RuntimeException("Unbound variable " + id)
          case Some(v) => v
        }
      }
      case Call(name, args) =>
        name match {
          case Ref(name) => env.get(name) match {
            case None => throw new RuntimeException("Undefined function " + name)
            case Some(v) => v.contents match {
              case None => throw new RuntimeException("Undefined function " + name)
              case Some(SFunc(params, body, closure)) =>
                interpExp(body, mapArgsToEnv(params zip args, closure, env))
              case Some(Primitive(operation)) => 
                operation match{
                  case "+" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SInt(val1), SInt(val2)) => SInt(val1 + val2)
                      case _ => throw new UnsupportedOperationException("Unsupported addition operation")
                    }
                  case "-" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SInt(val1), SInt(val2)) => SInt(val1 - val2)
                      case _ => throw new UnsupportedOperationException("Unsupported subtraction operation")
                    }
                  case "/" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SInt(val1), SInt(val2)) => SInt(val1 / val2)
                      case _ => throw new UnsupportedOperationException("Unsupported division operation")
                    }
                  case "*" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SInt(val1), SInt(val2)) => SInt(val1 * val2)
                      case _ => throw new UnsupportedOperationException("Unsupported multiplication operation")
                    }
                  case ">" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SInt(val1), SInt(val2)) => if (val1 > val2) STrue() else SFalse()
                      case _ => throw new UnsupportedOperationException("Unsupported greater than operation")
                    }
                  case "<" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SInt(val1), SInt(val2)) => if (val1 < val2) STrue() else SFalse()
                      case _ => throw new UnsupportedOperationException("Unsupported less than operation")
                    }
                  case ">=" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SInt(val1), SInt(val2)) => if (val1 >= val2) STrue() else SFalse()
                      case _ => throw new UnsupportedOperationException("Unsupported greater than or equal to operation")
                    }
                  case "<=" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SInt(val1), SInt(val2)) => if (val1 <= val2) STrue() else SFalse()
                      case _ => throw new UnsupportedOperationException("Unsupported less than or equal to operation")
                    }
                  case "cons" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(head: SExp, tail: SExp) => SCons(head, tail)
                      case _ => throw new UnsupportedOperationException("Unsupported cons opertaion")
                    }
                  case "car" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SCons(head, tail)) => head
                      case List(SNil) => SNil
                      case _ => throw new UnsupportedOperationException("Unsupported car operation")
                    }
                  case "cdr" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SCons(head, tail)) => tail
                      case List(SNil) => SNil
                      case _ => throw new UnsupportedOperationException("Unsupported cdr operation")
                    }
                  case "equal?" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(l: SFunc, r: SFunc) => throw new RuntimeException("Cannot compare function")
                      case List(l: Primitive, r: Primitive) => throw new RuntimeException("Cannot compare primitives")
                      case List(l: SExp, r: SExp) => if (l == r) STrue() else SFalse()
                      case _ => throw new RuntimeException("Unsupported comparison")
                    }
                  case "pair?" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SCons(head, tail)) => STrue()
                      case _ => SFalse()
                    }
                  case "null?" =>
                    args.map(e => interpExp(e, env)) match {
                      case List(SNil) => STrue()
                      case _ => SFalse()
                    }
                }
              case Some(v) => throw new RuntimeException(v + " is not a function")
            }
          }
          case Lambda(params, body) => interpExp(Lambda(params, body), env) match {
            case SFunc(params, body, closure) =>
              interpExp(body, mapArgsToEnv(params zip args, closure, env))
          }
          case exp: Exp => interpExp(exp, env) match {
            case SFunc(params, body, closure) =>
              interpExp(body, mapArgsToEnv(params zip args, closure, env))
            case _ => throw new RuntimeException(exp + " is not a valid function id")
          } 
        }
      case Lambda(params, body) => SFunc(params, body, env)
      case Print(exp) => {
        val res = interpExp(exp, env)
        println(res)
        SNil
      }
    }

  def checkEqualEh(l: Exp, r: Exp, env: Env): SExp =
    (interpExp(l, env), interpExp(r, env)) match {
      case (l: SFunc, r: SFunc) => throw new RuntimeException("Cannot compare functions")
      case (l: SExp, r: SExp) => if (l == r) STrue() else SFalse()
      case _ => throw new RuntimeException("Unknown type")
    }

  // TODO inline this to a foldLeft above
  def mapArgsToEnv(paramToArgs: List[(String, Exp)], acc: Env, env: Env) : Env =
    paramToArgs match {
      case Nil => acc
      case first :: rest =>
        mapArgsToEnv(rest, acc + (first._1 -> new Box(Some(interpExp(first._2, env)))), env)
    }

  def initFuncs(p: Program, env: Env) : Env =
    p.defs match {
      case Nil => env
      case first :: rest => initFuncs(
        Program(rest, p.exp),
        env + (first.name -> new Box()))
    }

  def defineFuncs(p: Program, env: Env) : Env = {
    p.defs.foreach { defItem =>
      env.get(defItem.name) match {
        case None => throw new RuntimeException("This shouldn't happen")
        case Some(v) => v.contents = Some(SFunc(defItem.params, defItem.body, env))
      }
    }
    env
  }

  def runProgram(p: Program, env: Env) : SExp =
    p.defs match {
      case Nil => interpExp(p.exp, env)
      case first :: rest =>
        runProgram(
          Program(rest, p.exp),
          env + (first.name -> new Box(Some(SFunc(first.params, first.body, env))))
        )
    }

  val initialEnv = Map(
      "+" -> new Box[SExp](Some(Primitive("+"))),
      "-" -> new Box[SExp](Some(Primitive("-"))),
      "/" -> new Box[SExp](Some(Primitive("/"))),
      "*" -> new Box[SExp](Some(Primitive("*"))),
      ">" -> new Box[SExp](Some(Primitive(">"))),
      "<" -> new Box[SExp](Some(Primitive("<"))),
      ">=" -> new Box[SExp](Some(Primitive(">="))),
      "<=" -> new Box[SExp](Some(Primitive("<="))),
      "cons" -> new Box[SExp](Some(Primitive("cons"))),
      "car" -> new Box[SExp](Some(Primitive("car"))),
      "cdr" -> new Box[SExp](Some(Primitive("cdr"))),
      "equal?" -> new Box[SExp](Some(Primitive("equal?"))),
      "pair?" -> new Box[SExp](Some(Primitive("pair?"))),
      "null?" -> new Box[SExp](Some(Primitive("null?")))
    ) 

  def interpProgram(p: Program, e: Env) : SExp = {
    var env = initialEnv ++ e
    runProgram(p, defineFuncs(p, initFuncs(p, env)))
  }

  def evalProgram(s: String) : SExp = 
    interpProgram(parseProgram(parseSExp(s)), Map())
}
