import sexp._
import expr._

object testExpr {
  def main(args: Array[String]): Unit = {
    // Arithmetic tests
    val arithEx1 = parseExp(parseSExp("(* (+ 5 2) 1)"))
    assert(interpExp(arithEx1, Map()) == SInt(7))

    val arithEx2 = parseExp(parseSExp("(/ (- 5 1) 2)"))
    assert(interpExp(arithEx2, Map()) == SInt(2))

    // Let tests
    val letEx1 = parseExp(parseSExp(
      """(let ((x 5))
       (+ x 7))"""))
    assert(interpExp(letEx1, Map()) == SInt(12))

    val letEx2 = parseExp(parseSExp(
      """(let ((x (+ 5 6)))
       (+ x 7))"""))
    assert(interpExp(letEx2, Map()) == SInt(18))

    val letEx3 = parseExp(parseSExp(
      """(let ((x (let ((y 1))
                 (+ y 2))))
        (+ x 3))"""))
    assert(interpExp(letEx3, Map()) == SInt(6))

    // Program parsing tests. We'll represent a program
    // as a sequence of defintions, followed by a main
    // expression which may call the defined functions.
    val progEx1 = parseProgram(parseSExp(
      """
      ((define (square n)
         (* n n))
       (square 5))
      """))

    assert(progEx1 ==
      Program(List(
        Def("square", List("n"), Multiply(Ref("n"), Ref("n")))
      ),
        Call("square", List(Literal(5))))
    )

    assert(interpProgram(progEx1, Map()) == SInt(25))

    val progEx2 = parseProgram(parseSExp(
      """
      ((define (square n)
         (* n n))
       (define (cube n)
         (* n (square n)))
       (cube 5))
      """))

    assert(progEx2 ==
      Program(List(
        Def("square", List("n"), Multiply(Ref("n"), Ref("n"))),
        Def("cube", List("n"), Multiply(Ref("n"), Call("square", List(Ref("n")))))
      ),
        Call("cube", List(Literal(5))))
    )

    assert(interpProgram(progEx2, Map()) == SInt(125))

    val progEx3 = parseProgram(parseSExp(
      """
      ((define (add x y)
         (+ x y))
       (add 5 6))
      """))

    assert(progEx3 ==
      Program(List(
        Def("add", List("x", "y"), Add(Ref("x"), Ref("y")))
      ),
        Call("add", List(Literal(5), Literal(6))))
    )

    assert(interpProgram(progEx3, Map()) == SInt(11))

    // Test a function definition with no params
    val progEx4 = parseProgram(parseSExp(
      """
      (
        (define (two) 2)
        (+ (two) 1)
      )
      """))

    assert(progEx4 ==
      Program(List(
        Def("two", List(), Literal(2))
      ), Add(Call("two", List()), Literal(1)))
    )

    assert(interpProgram(progEx4, Map()) == SInt(3))

    val progEx5 = parseProgram(parseSExp(
      """
      (
        (if #t 1 2)
      )
      """))

    assert(progEx5 ==
      Program(List(), If(True, Literal(1), Literal(2)))
    )

    assert(interpProgram(progEx5, Map()) == SInt(1))

    val progEx6 = parseProgram(parseSExp(
      """
      (
        (define (even? n)
          (if (equal? n 0)
            #t
           (odd? (- n 1))))
        (define (odd? n)
          (if (equal? n 0)
            #f
            (even? (- n 1))))
        (even? 10)
      )
      """))

    assert(progEx6 ==
      Program(
        List(
          Def(
            "even?",
            List("n"),
            If(EqualEh(Ref("n"), Literal(0)), True, Call("odd?", List(Subtract(Ref("n"), Literal(1)))))
          ),
          Def(
            "odd?",
            List("n"),
            If(EqualEh(Ref("n"), Literal(0)), False, Call("even?", List(Subtract(Ref("n"), Literal(1)))))
          )
        ),
        Call("even?", List(Literal(10)))
      )
    )

    assert(interpProgram(progEx6, Map()) == STrue())

    // First-class functions
    val progEx7 = parseProgram(parseSExp(
      """
      (
        (define (square n) (* n n))
        (define (callFuncWith2 func) (func 2))
        (callFuncWith2 square)
      )
      """))

    assert(progEx7 ==
      Program(
        List(
          Def(
            "square",
            List("n"),
            Multiply(Ref("n"), Ref("n"))
          ),
          Def(
            "callFuncWith2",
            List("func"),
            Call("func", List(Literal(2)))
          )
        ),
        Call("callFuncWith2", List(Ref("square")))
      )
    )

    assert(interpProgram(progEx7, Map()) == SInt(4))

    val progEx8 = parseProgram(parseSExp(
      """
      (
        (define 
          (append l s)
          (if (null? l)
            s
            (cons (car l) (append (cdr l) s))
          )
        )
        (append (quote (1 2 3)) (quote (4 5 6)))
      )
      """))

    assert(progEx8 ==
      Program(
        List(
          Def(
            "append",
            List("l", "s"),
            If(
              NullEh(Ref("l")),
              Ref("s"),
              Cons(
                Car(Ref("l")),
                Call(
                  "append",
                  List(Cdr(Ref("l")) , Ref("s"))
                )
              )
            )
          )
        ),
        Call(
          "append",
          List(
            Quote(SList(SInt(1), SInt(2), SInt(3))),
            Quote(SList(SInt(4), SInt(5), SInt(6)))
          )
        )
      )
    )

    assert(interpProgram(progEx8, Map()) == SList(SInt(1), SInt(2), SInt(3), SInt(4), SInt(5), SInt(6)))

    val progEx9 = parseProgram(parseSExp(
      """
      (
      (let ((f (lambda (x y) (+ x y)))) (f 1 2))
      )
      """))

    assert(progEx9 ==
      Program(
        List(),
        Let(
          List(Var("f", Lambda(List("x", "y"), Add(Ref("x"), Ref("y"))))),
          Call("f", List(Literal(1), Literal(2)))
        )
      )
    )

    assert(interpProgram(progEx9, Map()) == SInt(3))

  }
}
