package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class derivarParalelaTest extends AnyFunSuiteLike {

  val n = new Newton()
  val np = new NewtonParalela()

  test("test Derivar 1") {
    val expr = Suma(Prod(Atomo('x'), Suma(Atomo('x'), Numero(2))), Expo(Logaritmo(Atomo('x')), Numero(2)))
    assert(n.mostrar(np.derivarParalela(expr, Atomo('x'),0,2)) == "(((1.0 * (x + 2.0)) + (x * (1.0 + 0.0))) + " +
      "(2.0 * (((lg(x)) ^ 1.0) * (1.0 / x))))")
  }

  test("test Derivar 2"){
    val expr = Resta(
      Div(
        Prod(
          Suma(
            Expo(
              Atomo('x'),
              Numero(2)
            ),
            Atomo('y')
          ),
          Resta(
            Logaritmo(
              Atomo('x')
            ),
            Numero(3)
          )
        ),
        Atomo('x')
      ),
      Logaritmo(
        Div(
          Expo(
            Atomo('x'),
            Atomo('y')
          ),
          Atomo('y')
        )
      )
    )
    assert(
      n.mostrar(np.derivarParalela(expr, Atomo('x'),0,2)) ==
        "((((((((2.0 * ((x ^ 1.0) * 1.0)) + 0.0) * ((lg(x)) - 3.0)) + " +
          "(((x ^ 2.0) + y) * ((1.0 / x) - 0.0))) * x) - ((((x ^ 2.0) + y) * " +
          "((lg(x)) - 3.0)) * 1.0)) / (x * x)) - ((((((x ^ (y - 1.0)) * ((1.0 * y) + " +
          "(x * (0.0 * (lg(x)))))) * y) - ((x ^ y) * 0.0)) / (y * y)) / ((x ^ y) / y)))"
    )
  }


  test("test Derivar 3"){
    val expr = Suma(
      Prod(
        Resta(
          Expo(
            Suma(
              Logaritmo(
                Atomo('x')
              ),
              Atomo('y')
            ),
            Numero(2)
          ),
          Div(
            Atomo('x'),
            Numero(3)
          )
        ),
        Atomo('x')
      ),
      Div(
        Logaritmo(
          Expo(
            Atomo('x'),
            Atomo('y')
          )
        ),
        Atomo('y')
      )
    )
    assert(
      n.mostrar(np.derivarParalela(expr, Atomo('x'),0,2)) ==
        "(((((2.0 * ((((lg(x)) + y) ^ 1.0) * ((1.0 / x) + 0.0))) - (((1.0 * 3.0) - " +
          "(x * 0.0)) / (3.0 * 3.0))) * x) + (((((lg(x)) + y) ^ 2.0) - (x / 3.0)) * 1.0)) + " +
          "((((((x ^ (y - 1.0)) * ((1.0 * y) + (x * (0.0 * (lg(x)))))) / (x ^ y)) * y) - " +
          "((lg((x ^ y))) * 0.0)) / (y * y)))"
    )
  }

  test("test Derivar 4"){
    val expr = Resta(
      Div(
        Prod(
          Suma(
            Expo(
              Atomo('x'),
              Numero(2)
            ),
            Atomo('y')
          ),
          Resta(
            Logaritmo(
              Atomo('x')
            ),
            Numero(3)
          )
        ),
        Atomo('x')
      ),
      Logaritmo(
        Div(
          Expo(
            Atomo('x'),
            Atomo('y')
          ),
          Atomo('y')
        )
      )
    )
    assert(
      n.mostrar(np.derivarParalela(expr, Atomo('x'),0,2)) ==
        "((((((((2.0 * ((x ^ 1.0) * 1.0)) + 0.0) * ((lg(x)) - 3.0)) + (((x ^ 2.0) + y) * " +
          "((1.0 / x) - 0.0))) * x) - ((((x ^ 2.0) + y) * ((lg(x)) - 3.0)) * 1.0)) / (x * x)) " +
          "- ((((((x ^ (y - 1.0)) * ((1.0 * y) + (x * (0.0 * (lg(x)))))) * y) - ((x ^ y) * 0.0)) " +
          "/ (y * y)) / ((x ^ y) / y)))"
    )
  }

  test("test Derivar 5") {
    val expr = Suma(
      Prod(
        Resta(
          Expo(
            Suma(
              Logaritmo(
                Atomo('x')
              ),
              Atomo('y')
            ),
            Numero(2)
          ),
          Div(
            Atomo('x'),
            Numero(3)
          )
        ),
        Atomo('x')
      ),
      Div(
        Logaritmo(
          Expo(
            Atomo('x'),
            Atomo('y')
          )
        ),
        Atomo('y')
      )
    )
    assert(
      n.mostrar(np.derivarParalela(expr, Atomo('x'),0,2)) ==
        "(((((2.0 * ((((lg(x)) + y) ^ 1.0) * ((1.0 / x) + 0.0))) - (((1.0 * 3.0) - " +
          "(x * 0.0)) / (3.0 * 3.0))) * x) + (((((lg(x)) + y) ^ 2.0) - (x / 3.0)) * 1.0)) + " +
          "((((((x ^ (y - 1.0)) * ((1.0 * y) + (x * (0.0 * (lg(x)))))) / (x ^ y)) * y) - " +
          "((lg((x ^ y))) * 0.0)) / (y * y)))"
    )
  }

}
