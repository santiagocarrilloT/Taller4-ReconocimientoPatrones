package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class buenaAproxTest extends AnyFunSuiteLike {
  val n = new Newton()
  test("testBuenaAprox 1") {
    val expr = Suma(Numero(0.0005), Numero(0.0004))
    assert(n.buenaAprox(expr, Atomo('x'), 0.0))
  }

  test("testBuenaAprox 2") {
    val expr2 = Suma(
      Prod(
        Resta(
          Expo(
            Suma(
              Logaritmo(
                Atomo('x')
              ),
              Atomo('x')
            ),
            Numero(3)
          ),
          Div(
            Atomo('x'),
            Numero(4)
          )
        ),
        Atomo('x')
      ),
      Div(
        Logaritmo(
          Expo(
            Atomo('x'),
            Atomo('x')
          )
        ),
        Atomo('x')
      )
    )
    assert(n.buenaAprox(expr2, Atomo('x'), 1.5))
  }

  test("testBuenaAprox 3") {
    val expr3 = Suma(
      Prod(
        Resta(
          Expo(
            Suma(
              Logaritmo(
                Atomo('x')
              ),
              Atomo('x')
            ),
            Numero(4)
          ),
          Div(
            Atomo('x'),
            Numero(5)
          )
        ),
        Atomo('x')
      ),
      Div(
        Logaritmo(
          Expo(
            Atomo('x'),
            Atomo('x')
          )
        ),
        Atomo('x')
      )
    )
    assert(n.buenaAprox(expr3, Atomo('x'), 2.5))
  }

  test("testBuenaAprox 4") {
    val expr4 = Suma(
      Prod(
        Resta(
          Expo(
            Suma(
              Logaritmo(
                Atomo('x')
              ),
              Atomo('x')
            ),
            Numero(5)
          ),
          Div(
            Atomo('x'),
            Numero(6)
          )
        ),
        Atomo('x')
      ),
      Div(
        Logaritmo(
          Expo(
            Atomo('x'),
            Atomo('x')
          )
        ),
        Atomo('x')
      )
    )
    assert(n.buenaAprox(expr4, Atomo('x'), 4.0))
  }

  test("testBuenaAprox 5") {
    val expr5 = Suma(
      Prod(
        Resta(
          Expo(
            Suma(
              Logaritmo(
                Atomo('x')
              ),
              Atomo('x')
            ),
            Numero(6)
          ),
          Div(
            Atomo('x'),
            Numero(7)
          )
        ),
        Atomo('x')
      ),
      Div(
        Logaritmo(
          Expo(
            Atomo('x'),
            Atomo('x')
          )
        ),
        Atomo('x')
      )
    )
    assert(n.buenaAprox(expr5, Atomo('x'), 5.0))
  }
}
