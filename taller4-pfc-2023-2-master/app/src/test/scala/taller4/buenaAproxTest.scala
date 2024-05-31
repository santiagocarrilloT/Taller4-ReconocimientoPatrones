package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class buenaAproxTest extends AnyFunSuiteLike {
  val n = new Newton()
  test("testBuenaAprox 1") {
    val expr1 = Resta(Expo(Atomo('x'), Numero(2)), Numero(4))
    assert(n.buenaAprox(expr1, Atomo('x'), 2.0))
  }

  test("testBuenaAprox 2") {
    val expr2 = Resta(Logaritmo(Atomo('x')), Numero(1))
    assert(n.buenaAprox(expr2, Atomo('x'), math.exp(1)))
  }

  test("testBuenaAprox 3") {
    val expr3 = Resta(Expo(Atomo('x'), Numero(2)), Numero(4))
    assert(n.buenaAprox(expr3, Atomo('x'), 2.0))
  }

  test("testBuenaAprox 4") {
    val expr4 = Resta(Logaritmo(Atomo('x')), Numero(1))
    assert(n.buenaAprox(expr4, Atomo('x'), math.exp(1) + 0.001))
  }

  test("testBuenaAprox 5") {
    val expr5 = Resta(Expo(Atomo('x'), Numero(2)), Numero(4))
    assert(n.buenaAprox(expr5, Atomo('x'), 2.0001))
  }
}
