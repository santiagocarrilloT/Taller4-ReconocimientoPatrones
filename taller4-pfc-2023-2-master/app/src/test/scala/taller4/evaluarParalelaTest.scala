package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class evaluarParalelaTest extends AnyFunSuiteLike {


  val np = new NewtonParalela()

  test("testEvaluar 1") {
    val expr1 = Suma(
      Expo(
        Atomo('x'),Numero(3)), Suma(Prod(Numero(6), Expo( Atomo('x'), Numero(2))),Resta(Prod(Numero(11), Atomo('x')), Numero(6))) )
    assert(np.evaluarParalela(expr1, Atomo('x'), 3.0, 0 , 1) == 108.0)
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
    assert(np.evaluarParalela(expr, Atomo('x'), 2.0, 0, 1) == -7.613705638880109)
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
    assert(np.evaluarParalela(expr, Atomo('x'), 4.0,0,1) == 41.92091902839603)
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
    assert(np.evaluarParalela(expr, Atomo('x'), 3.0,0,1) == -2.7777777777777777)
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
    assert(np.evaluarParalela(expr, Atomo('x'), 2.0,0,1) == -22.85390081777927)
  }


}
