package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class mostrarTest extends AnyFunSuiteLike {
  val n = new Newton()

  test("test mostrar 1"){
    val expr1 = Prod(Suma(Prod(Expo(Atomo('x'), Numero(2)), Numero(2)), Resta(Atomo('x'), Numero(1))), Suma(Prod(Expo(Atomo('x'), Numero(3)), Numero(1)), Resta(Suma(Prod(Numero(3), Expo(Atomo('x'), Numero(2))), Prod(Numero(2), Atomo('x'))), Numero(4))))
    assert(n.mostrar(n.limpiar(expr1)) == "((((x ^ 2.0) * 2.0) + (x - 1.0)) * ((x ^ 3.0) + (((3.0 * (x ^ 2.0)) + (2.0 * x)) - 4.0)))" )
  }

  test("test mostrar 2"){
    val expr2 = Prod(Suma(Prod(Expo(Atomo('x'), Numero(3)), Numero(-2)), Suma(Expo(Atomo('x'), Numero(2)), Prod(Numero(3), Atomo('x')))), Suma(Prod(Expo(Atomo('x'), Numero(4)), Numero(4)), Suma(Prod(Numero(-5), Expo(Atomo('x'), Numero(2))), Suma(Prod(Numero(6), Atomo('x')), Numero(-7)))))
    assert(n.mostrar(expr2) == "((((x ^ 3.0) * -2.0) + ((x ^ 2.0) + (3.0 * x))) * (((x ^ 4.0) * 4.0) + ((-5.0 * (x ^ 2.0)) + ((6.0 * x) + -7.0))))")
  }

  test("test mostrar 3") {
    val expr3 = Prod(Suma(Prod(Expo(Atomo('x'), Numero(4)), Numero(2)), Resta(Expo(Atomo('x'), Numero(3)), Resta(Prod(Atomo('x'), Numero(3)), Numero(6)))), Suma(Prod(Expo(Atomo('x'), Numero(3)), Numero(1)), Resta(Suma(Prod(Numero(3), Expo(Atomo('x'), Numero(2))), Prod(Numero(2), Atomo('x'))), Numero(4))))
    assert(n.mostrar(expr3) == "((((x ^ 4.0) * 2.0) + ((x ^ 3.0) - ((x * 3.0) - 6.0))) * (((x ^ 3.0) * 1.0) + (((3.0 * (x ^ 2.0)) + (2.0 * x)) - 4.0)))")
  }

  test("test mostrar 4"){
    val expr4 = Prod(Suma(Prod(Expo(Atomo('x'), Numero(5)), Numero(-5)), Suma(Prod(Numero(10), Expo(Atomo('x'), Numero(3))), Resta(Suma(Prod(Numero(10), Expo(Atomo('x'), Numero(2))), Prod(Numero(5), Atomo('x'))), Numero(1)))), Suma(Prod(Expo(Atomo('x'), Numero(4)), Numero(1)), Resta(Suma(Prod(Numero(-4), Expo(Atomo('x'), Numero(3))), Prod(Numero(6), Expo(Atomo('x'), Numero(2)))), Resta(Prod(Numero(4), Atomo('x')), Numero(1)))))
    assert(n.mostrar(expr4) == "((((x ^ 5.0) * -5.0) + ((10.0 * (x ^ 3.0)) + (((10.0 * (x ^ 2.0)) + (5.0 * x)) - 1.0))) * (((x ^ 4.0) * 1.0) + (((-4.0 * (x ^ 3.0)) + (6.0 * (x ^ 2.0))) - ((4.0 * x) - 1.0))))")
  }


  test("test mostrar 5") {
    val expr5 = Prod(Suma(Prod(Expo(Atomo('x'), Numero(6)), Numero(-6)), Suma(Prod(Numero(15), Expo(Atomo('x'), Numero(4))), Resta(Suma(Prod(Numero(20), Expo(Atomo('x'), Numero(3))), Prod(Numero(15), Expo(Atomo('x'), Numero(2)))), Prod(Numero(6), Atomo('x'))))), Suma(Prod(Expo(Atomo('x'), Numero(5)), Numero(1)), Resta(Suma(Prod(Numero(-5), Expo(Atomo('x'), Numero(4))), Prod(Numero(10), Expo(Atomo('x'), Numero(3)))), Resta(Suma(Prod(Numero(10), Expo(Atomo('x'), Numero(2))), Prod(Numero(5), Atomo('x'))), Numero(1)))))
    assert(n.mostrar(expr5)  == "((((x ^ 6.0) * -6.0) + ((15.0 * (x ^ 4.0)) + (((20.0 * (x ^ 3.0)) + (15.0 * (x ^ 2.0))) - (6.0 * x)))) * (((x ^ 5.0) * 1.0) + (((-5.0 * (x ^ 4.0)) + (10.0 * (x ^ 3.0))) - (((10.0 * (x ^ 2.0)) + (5.0 * x)) - 1.0))))")
  }
}
