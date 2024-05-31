package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class raizNewtonTest extends AnyFunSuiteLike {
  val n = new Newton()

  test("testRaizNewton 1") {
    val expr1 = Resta(Expo(Atomo('x'), Numero(2)), Numero(4))
    assert(math.abs(n.raizNewton(expr1, Atomo('x'), 1.0, n.buenaAprox) - 2.0) < 0.001)
  }
  test("testRaizNewton 2") {
    val expr2 = Resta(Expo(Atomo('x'), Numero(3)), Numero(27))
    assert(math.abs(n.raizNewton(expr2, Atomo('x'), 1.0, n.buenaAprox) - 3.0) < 0.001)
  }
  test("testRaizNewton 3") {
    val expr1 = Resta(Expo(Atomo('x'), Numero(2)), Numero(4))
    assert(math.abs(n.raizNewton(expr1, Atomo('x'), 1.0, n.buenaAprox) - 2.0) < 0.001)
  }
  test("testRaizNewton 4") {
    val expr1 = Resta(Expo(Atomo('x'), Numero(2)), Numero(4))
    assert(math.abs(n.raizNewton(expr1, Atomo('x'), 1.0, n.buenaAprox) - 2.0) < 0.001)
  }
  test("testRaizNewton 5") {
    val expr1 = Resta(Expo(Atomo('x'), Numero(2)), Numero(4))
    assert(math.abs(n.raizNewton(expr1, Atomo('x'), 1.0, n.buenaAprox) - 2.0) < 0.001)
  }

}
