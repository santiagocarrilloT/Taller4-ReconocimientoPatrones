package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class mostrarTest extends AnyFunSuiteLike {
  val n = new Newton()

  test("test mostrar 1"){
    val expr1 = n.derivar(Suma(Atomo ('n'),Prod(Numero(3.0),Atomo('x'))),Atomo('x'))
    assert(n.mostrar(n.limpiar(expr1)) == "3.0" )
  }

  test("test mostrar 2"){
    val expr1 = Suma(Prod(Numero(2), Atomo('x')), Resta(Numero(4), Div(Numero(9), Numero(3))))
    assert(n.mostrar(expr1) == "((2.0 * x) + (4.0 - (9.0 / 3.0)))")
  }

  test("test mostrar 3") {
    val expr = Expo(
      Div(
        Prod(
          Suma(Numero(2), Logaritmo(Atomo('x'))),
          Resta(Numero(4), Atomo('y'))
        ),
        Numero(3)
      ),
      Atomo('z')
    )
    assert(n.mostrar(expr) == "((((2.0 + (lg(x))) * (4.0 - y)) / 3.0) ^ z)")
  }

  test("test mostrar 4"){
    val expr3 = Div(Prod(Suma(Atomo('y'), Numero(5)), Atomo('x')), Expo(Numero(3), Atomo('z')))
    assert(n.mostrar(expr3) == "(((y + 5.0) * x) / (3.0 ^ z))")
  }


  test("test mostrar 5") {
    val expr = Suma(Prod(Numero(2), Atomo('x')), Div(Numero(10), Atomo('y')))
    assert(n.mostrar(expr)  == "((2.0 * x) + (10.0 / y))")
  }
}
