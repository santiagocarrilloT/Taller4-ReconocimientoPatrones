package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class buenaAproxTest extends AnyFunSuiteLike {
  val n = new Newton()
  test("testBuenaAprox 1") {
    val expr = Resta(Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x')), Resta(Suma(Prod(Numero(6), Prod(Atomo('x'), Atomo('x'))), Prod(Numero(11), Atomo('x'))), Numero(6)))
    assert(n.buenaAprox(expr, Atomo('x'), 1.0))
  }

  test("testBuenaAprox 2") {
    val expr2 = Resta(Prod(Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x')), Atomo('x')), Suma(Suma(Suma(Prod(Numero(10), Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x'))), Prod(Numero(35), Prod(Atomo('x'), Atomo('x')))), Prod(Numero(50), Atomo('x'))), Numero(24)))
    assert(n.buenaAprox(expr2, Atomo('x'), 1.0))
    assert(n.buenaAprox(expr2, Atomo('x'), 2.0))
    assert(n.buenaAprox(expr2, Atomo('x'), 3.0))
    assert(n.buenaAprox(expr2, Atomo('x'), 4.0))
  }

  test("testBuenaAprox 3") {
    val expr3 = Resta(Logaritmo(Atomo('x')), Numero(2))
    assert(n.buenaAprox(expr3, Atomo('x'), 7.389))
  }

  test("testBuenaAprox 4") {
    val expr4 = Resta(Expo(Numero(math.E), Atomo('x')), Numero(10))
    assert(n.buenaAprox(expr4, Atomo('x'), 2.302))
  }

  test("testBuenaAprox 5") {
    val expr5 = Resta(Expo(Numero(2), Atomo('x')), Numero(10))
    assert(n.buenaAprox(expr5, Atomo('x'), 3.321))

  }
}
