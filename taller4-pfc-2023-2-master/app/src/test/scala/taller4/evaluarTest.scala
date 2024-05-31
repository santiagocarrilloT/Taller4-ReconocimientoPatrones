package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class evaluarTest extends AnyFunSuiteLike {
  val n = new Newton()
  test("testEvaluar 1") {
    val expr = Expo(Logaritmo(Numero(10)), Numero(2))
    assert(n.evaluar(expr, Atomo('x'), 0) == math.pow(math.log(10), 2))
  }
  test("testEvaluar 2") {
    val expr = Div(Suma(Numero(10), Atomo('x')), Numero(5))
    assert(n.evaluar(expr, Atomo('x'), 5) == 3)
  }
  test("testEvaluar 3") {
    val expr = Prod(Atomo('x'), Suma(Numero(5), Atomo('x')))
    assert(n.evaluar(expr, Atomo('x'), 3) == 24)
  }
  test("testEvaluar 4") {
    val expr = Resta(Prod(Numero(7), Atomo('x')), Prod(Numero(2), Numero(5)))
    assert(n.evaluar(expr, Atomo('x'), 2) == 4)
  }
  test("testEvaluar 5") {
    val expr = Suma(Prod(Numero(2), Atomo('x')), Expo(Numero(3), Numero(2)))
    assert(n.evaluar(expr, Atomo('x'), 4) == 17)
  }

}
