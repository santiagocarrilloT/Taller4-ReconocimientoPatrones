package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class limpiarTest extends AnyFunSuiteLike {
  val n = new Newton()

  test("Test Limpiar 1"){
    val expr = Suma(Resta(Atomo('x'), Atomo('x')), Suma(Numero(0), Numero(5)))
    val exprLimpia = n.limpiar(expr)
    assert(exprLimpia == Numero(5))
  }
  test("Test Limpiar 2"){
    val expr = Resta(
      Suma(
        Prod(
          Suma(
            Prod(Numero(2), Atomo('x')),
            Numero(3)
          ),
          Expo(Atomo('x'), Numero(4))
        ),
        Suma(
          Prod(Numero(3), Atomo('x')),
          Numero(2)
        )
      ),
      Div(
        Logaritmo(
          Suma(
            Prod(Numero(4), Expo(Atomo('x'), Numero(3))),
            Suma(
              Prod(Numero(2), Atomo('x')),
              Numero(1)
            )
          )
        ),
        Suma(
          Prod(Numero(6), Atomo('x')),
          Numero(7)
        )
      )
    )
    assert(n.mostrar(n.limpiar(n.derivar(expr, Atomo('x')))) == "((((2.0*(x^4.0))+(((2.0*x)+3.0)*(4.0*(x^3.0))))+3.0)-((((((4.0*(3.0*(x^2.0)))+2.0)/((4.0*(x^3.0))+((2.0*x)+1.0)))*((6.0*x)+7.0))-((lg(((4.0*(x^3.0))+((2.0*x)+1.0))))*6.0))/(((6.0*x)+7.0)*((6.0*x)+7.0))))")
  }
  test("Test Limpiar 3"){
    val expr = Suma(
      Prod(
        Suma(
          Prod(Numero(2), Expo(Atomo('x'), Numero(3))),
          Prod(Numero(3), Expo(Atomo('x'), Numero(2)))
        ),
        Suma(
          Prod(Numero(5), Atomo('x')),
          Numero(6)
        )
      ),
      Div(
        Expo(Atomo('x'), Numero(5)),
        Suma(
          Prod(Numero(7), Atomo('x')),
          Numero(8)
        )
      )
    )
    assert(n.mostrar(n.limpiar(n.derivar(expr, Atomo('x')))) == "(((((2.0*(3.0*(x^2.0)))+(3.0*(2.0*x)))*((5.0*x)+6.0))+(((2.0*(x^3.0))+(3.0*(x^2.0)))*5.0))+((((5.0*(x^4.0))*((7.0*x)+8.0))-((x^5.0)*7.0))/(((7.0*x)+8.0)*((7.0*x)+8.0))))")
  }
  test("Test Limpiar 4"){
    val expr5 = Prod(
      Suma(
        Prod(
          Suma(
            Prod(Numero(2), Atomo('x')),
            Numero(3)
          ),
          Expo(Atomo('x'), Numero(4))
        ),
        Suma(
          Prod(Numero(3), Expo(Atomo('x'), Numero(2))),
          Numero(5)
        )
      ),
      Div(
        Logaritmo(
          Suma(
            Prod(Numero(4), Expo(Atomo('x'), Numero(3))),
            Suma(
              Prod(Numero(2), Atomo('x')),
              Numero(1)
            )
          )
        ),
        Suma(
          Prod(Numero(6), Atomo('x')),
          Numero(7)
        )
      )
    ) // 2 * x + (0 + 1 * y)
    assert(n.mostrar(n.limpiar(n.derivar(expr5, Atomo('x')))) == "(((((2.0[ * " +
      "(x ^ 4.0)) + (((2.0 * x) + 3.0) * (4.0 * (x ^ 3.0)))) + (3.0 * (2.0 * x))) " +
      "* ((lg(((4.0 * (x ^ 3.0)) + ((2.0 * x) + 1.0)))) / ((6.0 * x) + 7.0))) + " +
      "(((((2.0 * x) + 3.0) * (x ^ 4.0)) + ((3.0 * (x ^ 2.0)) + 5.0)) * ((((((4.0 * " +
      "(3.0 * (x ^ 2.0))) + 2.0) / ((4.0 * (x ^ 3.0)) + ((2.0 * x) + 1.0))) * " +
      "((6.0 * x) + 7.0)) - ((lg(((4.0 * (x ^ 3.0)) + ((2.0 * x) + 1.0)))) * 6.0)) " +
      "/ (((6.0 * x) + 7.0) * ((6.0 * x) + ]7.0)))))")
  }

  test("Test Limpiar 5") {
    val expr6 = Suma(
      Prod(
        Suma(
          Prod(Numero(2), Expo(Atomo('x'), Numero(3))),
          Prod(Numero(3), Expo(Atomo('x'), Numero(2)))
        ),
        Suma(
          Prod(Numero(5), Atomo('x')),
          Numero(6)
        )
      ),
      Div(
        Expo(Atomo('x'), Numero(5)),
        Suma(
          Prod(Numero(7), Atomo('x')),
          Numero(8)
        )
      )
    )
    assert(n.mostrar(n.limpiar(n.derivar(expr6, Atomo('x')))) == "(((((2.0[ * (3.0 * (x ^ 2.0))) + (3.0 * (2.0 * x))) * ((5.0 * x) + 6.0)) + (((2.0 * (x ^ 3.0)) + (3.0 * (x ^ 2.0))) * 5.0)) + ((((5.0 * (x ^ 4.0)) * ((7.0 * x) + 8.0)) - ((x ^ 5.0) * 7.0)) / (((7.0 * x) + 8.0) * ((7.0 * x) + ]8.0))))")
  }

}
