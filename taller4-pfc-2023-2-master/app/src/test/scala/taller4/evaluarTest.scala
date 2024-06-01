package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class evaluarTest extends AnyFunSuiteLike {
  val n = new Newton()
  test("testEvaluar 1") {
    val expr1 = Suma(
      Prod(
        Suma(Atomo('x'), Numero(1)),
        Div(
          Resta(Expo(Atomo('x'), Numero(2)), Numero(4)),
          Numero(2)
        )
      ),
      Div(Numero(5), Suma(Atomo('x'), Numero(2)))
    )
    val resultado = n.evaluar(expr1, Atomo('x'), 2.0)
    assert(resultado == 1.25)
  }

  test("testEvaluar 2") {
    val expr = Resta(
      Div(
        Prod(
          Suma(
            Expo(
              Atomo('x'),
              Numero(2)
            ),
            Numero(2)
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
            Numero(2)
          ),
          Numero(2)
        )
      )
    )
    val resultado = n.evaluar(expr, Atomo('x'), 2.0)
    assert(resultado == -7.613705638880109)
  }

  test("testEvaluar 3") {
    val expr = Suma(
      Prod(
        Resta(
          Expo(
            Suma(
              Logaritmo(
                Atomo('x')
              ),
              Numero(2)
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
            Numero(2)
          )
        ),
        Numero(2)
      )
    )
    val resultado = n.evaluar(expr, Atomo('x'), 4.0)
    assert(resultado == 41.92091902839603)
  }

  test("testEvaluar 4") {
    val expr = Resta(
      Div(
        Suma(
          Prod(
            Atomo('x'),
            Atomo('x')
          ),
          Numero(2)
        ),
        Expo(
          Atomo('x'),
          Numero(2)
        )
      ),
      Prod(
        Numero(2),
        Numero(2)
      )
    )
    val resultado = n.evaluar(expr, Atomo('x'), 3.0)
    assert(resultado == -2.7777777777777777)
  }

  test("testEvaluar 5") {
    val expr = Div(
      Resta(
        Prod(
          Suma(
            Expo(
              Atomo('x'),
              Numero(2)
            ),
            Numero(2)
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
            Numero(2)
          ),
          Numero(2)
        )
      )
    )
    val resultado = n.evaluar(expr, Atomo('x'), 2.0)
    assert(resultado == -22.85390081777927)
  }

}
