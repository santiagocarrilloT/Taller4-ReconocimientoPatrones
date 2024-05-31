package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class limpiarTest extends AnyFunSuiteLike {
  val n = new Newton()

  test("Test Limpiar 1"){
    val expr = Prod(Numero(1), Suma(Numero(0), Numero(3)))
    assert(n.limpiar(expr) == Numero(3))
  }
  test("Test Limpiar 2"){
    val expr = Prod(Numero(1), Div(Numero(0), Numero(1)))
    assert(n.limpiar(expr) == Numero(0))
  }
  test("Test Limpiar 3"){
    val expr = Suma(Numero(0), Resta(Numero(5), Numero(0)))
    assert(n.limpiar(expr) == Numero(5))
  }
  test("Test Limpiar 4"){
    val expr5 = Suma(Prod(Numero(2), Atomo('x')), Suma(Numero(0), Prod(Numero(1), Atomo('y')))) // 2 * x + (0 + 1 * y)
    assert(n.mostrar(n.limpiar(n.derivar(expr5, Atomo('x')))) == "2.0")
  }

  test("Test Limpiar 5") {
    val expr6 = Prod(Atomo('x'), Logaritmo(Atomo('x')))
    println(n.mostrar(n.limpiar(n.derivar(expr6, Atomo('x')))))
    assert(n.mostrar(n.limpiar(n.derivar(expr6, Atomo('x')))) == "((lg(x)) + (x * (1.0 / x)))")
  }

}
