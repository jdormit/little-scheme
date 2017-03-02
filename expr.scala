import sexp._

package object expr {
  case class SFunc(params: List[String], body: Exp) extends SExp

  sealed abstract class Exp
  case class Literal(v: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Subtract(lhs: Exp, rhs: Exp) extends Exp
  case class Multiply(lhs: Exp, rhs: Exp) extends Exp
  case class Divide(lhs: Exp, rhs: Exp) extends Exp
  case class Var(name: String, exp: Exp)
  case class Let(vars: List[Var], body: Exp) extends Exp
  case class Ref(v: String) extends Exp
  case class Call(name: String, args: List[Exp]) extends Exp
  case class If(condition: Exp, lhs: Exp, rhs: Exp) extends Exp
  case class EqualEh(lhs: Exp, rhs: Exp) extends Exp
  case object True extends Exp
  case object False extends Exp
  case class Cons(head: Exp, tail: Exp) extends Exp
  case class Car(cons: Exp) extends Exp
  case class Cdr(cons: Exp) extends Exp
  case class PairEh(exp: Exp) extends Exp
  case object Null extends Exp
  case class NullEh(exp: Exp) extends Exp
  case class Quote(exp: SExp) extends Exp
  case class Lambda(params: List[String], body: Exp) extends Exp

  case class Def(name: String, params: List[String], body: Exp)
  case class Program(defs: List[Def], exp: Exp)

  def parseExp(e: SExp) : Exp =
    e match {
      case SInt(v) => Literal(v)
      case STrue() => True
      case SFalse() => False
      case SSymbol("null") => Null
      case SList(SSymbol("+"), l, r) => Add(parseExp(l), parseExp(r))
      case SList(SSymbol("*"), l, r) => Multiply(parseExp(l), parseExp(r))
      case SList(SSymbol("-"), l, r) => Subtract(parseExp(l), parseExp(r))
      case SList(SSymbol("/"), l, r) => Divide(parseExp(l), parseExp(r))
      case SList(SSymbol("if"), cond, l, r) => If(parseExp(cond), parseExp(l), parseExp(r))
      case SList(SSymbol("equal?"), l, r) => EqualEh(parseExp(l), parseExp(r))
      case SSymbol(id) => Ref(id)
      case SList(
        SSymbol("let"),
        defs,
        body) => parseLet(defs, body, List())
      case SList(SSymbol("cons"), l, r) => Cons(parseExp(l), parseExp(r))
      case SList(SSymbol("car"), exp) => Car(parseExp(exp))
      case SList(SSymbol("cdr"), exp) => Cdr(parseExp(exp))
      case SList(SSymbol("pair?"), exp) => PairEh(parseExp(exp))
      case SList(SSymbol("null?"), exp) => NullEh(parseExp(exp))
      case SList(SSymbol("quote"), exp) => Quote(exp)
      case SList(SSymbol("lambda"), params, body) => parseLambda(params, body, List())
      case SCons(SSymbol(id), args) => parseCall(args, id, List())
      case _ => throw new IllegalArgumentException("Not a valid arithmetic expression: " + e)
    }

  // TODO the following three functions could be abstracted to a higher order function

  def parseLambda(params: SExp, body: SExp, acc: List[String]): Lambda =
    params match {
      case SNil => Lambda(acc.reverse, parseExp(body))
      case SCons(SSymbol(param), rest) => parseLambda(rest, body, param :: acc)
    }

  def parseCall(args: SExp, id: String, acc: List[Exp]) : Call =
    args match {
      case SNil => Call(id, acc.reverse)
      case SCons(first, rest) => parseCall(rest, id, parseExp(first) :: acc)
    }

  def parseLet(defs: SExp, body: SExp, vars: List[Var]) : Let =
    defs match {
      case SNil => Let(vars.reverse, parseExp(body))
      case SCons(first, rest) => first match {
        case SList(SSymbol(id), exp) => parseLet(rest, body, Var(id, parseExp(exp)) :: vars)
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
        case exp: SCons => Program(defs.reverse, parseExp(exp))
        case _ => throw new IllegalArgumentException("Not a valid program: " + e)
      }
    }

  type Env = Map[String, SExp]

  def interpExp(e: Exp, env: Env) : SExp =
    e match {
      case Literal(v) => SInt(v)
      case True => STrue()
      case False => SFalse()
      case Null => SNil
      case Cons(head, tail) => SCons(interpExp(head, env), interpExp(tail, env))
      case Quote(sexp) => sexp
      case Car(exp) => interpExp(exp, env) match {
        case SCons(head, tail) => head
        case _ =>
          throw new UnsupportedOperationException(
            "Car is not supported for type " + exp.getClass.getName
          )
      }
      case Cdr(exp) => interpExp(exp, env) match {
        case SCons(head, tail) => tail
        case _ =>
          throw new UnsupportedOperationException(
            "Cdr is not supported for types " + exp.getClass.getName
          )
      }
      case PairEh(exp) => interpExp(exp, env) match {
        case SCons(head, tail) => STrue()
        case _ => SFalse()
      }
      case NullEh(exp) => interpExp(exp, env) match {
        case SNil => STrue()
        case _ => SFalse()
      }
      case Add(l, r) => (interpExp(l, env), interpExp(r, env)) match {
        case (SInt(lv), SInt(rv)) => SInt(lv + rv)
        case _ =>
          throw new UnsupportedOperationException(
            "Addition is not supported for types " +
              l.getClass.getName + " and " +
              r.getClass.getName
          )
      }
      case Subtract(l, r) => (interpExp(l, env), interpExp(r, env)) match {
        case (SInt(lv), SInt(rv)) => SInt(lv - rv)
        case _ =>
          throw new UnsupportedOperationException(
            "Addition is not supported for types " +
              l.getClass.getName + " and " +
              r.getClass.getName
          )
      }
      case Multiply(l, r) => (interpExp(l, env), interpExp(r, env)) match {
        case (SInt(lv), SInt(rv)) => SInt(lv * rv)
        case _ =>
          throw new UnsupportedOperationException(
            "Multiplication is not supported for types " +
              l.getClass.getName + " and " +
              r.getClass.getName
          )
      }
      case Divide(l, r) => (interpExp(l, env), interpExp(r, env)) match {
        case (SInt(lv), SInt(rv)) => SInt(lv / rv)
        case _ =>
          throw new UnsupportedOperationException(
            "Addition is not supported for types " +
              l.getClass.getName + " and " +
              r.getClass.getName
          )
      }
      case If(condition, l, r) => interpExp(condition, env) match {
        case STrue() => interpExp(l, env)
        case SFalse() => interpExp(r, env)
        case _ => interpExp(l, env)
      }
      case EqualEh(l, r) => checkEqualEh(l, r, env)
      case Ref(id) => env.get(id) match {
        case None => throw new RuntimeException("Unbound variable " + id)
        case Some(v) => v
      }
      case Let(vars, body) => vars match {
        case Nil => interpExp(body, env)
        case first :: rest =>
          interpExp(Let(rest, body), env + (first.name -> interpExp(first.exp, env)))
      }
      case Call(name, args) => env.get(name) match {
        case None => throw new RuntimeException("Undefined function " + name)
        case Some(SFunc(params, body)) =>
          interpExp(body, mapArgsToEnv(params zip args, env))
        case Some(v) => throw new RuntimeException(v + " is not a function")
      }
      case Lambda(params, body) => SFunc(params, body)
    }

  def checkEqualEh(l: Exp, r: Exp, env: Env): SExp =
    (interpExp(l, env), interpExp(r, env)) match {
      case (SFunc(lparams, lbody), SFunc(rparams, rbody)) =>
        // If the parameters are the same, return checkEqualEh of the bodies
        if (lparams == rparams) checkEqualEh(lbody, rbody, env) else SFalse()
      case (l: SExp, r: SExp) => if (l == r) STrue() else SFalse()
      case _ => throw new RuntimeException("Unknown type")
    }

  // TODO inline this to a foldLeft above
  def mapArgsToEnv(paramToArgs: List[(String, Exp)], acc: Env) : Env =
    paramToArgs match {
      case Nil => acc
      case first :: rest =>
        mapArgsToEnv(rest, acc + (first._1 -> interpExp(first._2, acc)))
    }

  def interpProgram(p: Program, env: Env) : SExp =
    p.defs match {
      case Nil => interpExp(p.exp, env)
      case first :: rest => interpProgram(
        Program(rest, p.exp),
        env + (first.name -> SFunc(first.params, first.body)))
    }

  def evalExp(s: String) : SExp = interpExp(parseExp(parseSExp(s)), Map())

  def evalProgram(s: String) : SExp = interpProgram(parseProgram(parseSExp(s)), Map())

  """
  Example Expressions
  -------------------

  (let ( (x 4) (y 2) ) (+ x y))
  // => 6
  """

  """
  Example Programs
  --------
  ((define (square n)
     (* n n))
     (square 5))
  // => 25

  (
    (define (square n) (* n n))
    (define (cube n) (* n (square n)))
    (cube 5)
  )
  // => 125

  (
    (define (square n) (* n n))
    (define (reverse n) (* n -1))
    (+ 
      (let ((x 4)) (square x))
      (reverse 3)
    )
  )
  // => 13

  A program data structure:
  Program(List(Def("Square", "n", Multiply(Ref("n"), Ref("n"))))
  """
}
