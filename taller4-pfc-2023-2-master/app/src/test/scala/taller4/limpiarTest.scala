package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class limpiarTest extends AnyFunSuiteLike {
  val n = new Newton()

  test("Test Limpiar 1"){
    val expr = Suma(Prod(Suma(Prod(Expo(Atomo('x'), Numero(2)), Numero(2)), Resta(Atomo('x'), Numero(1))), Suma(Expo(Atomo('x'), Numero(3)), Resta(Suma(Prod(Numero(4), Expo(Atomo('x'), Numero(2))), Prod(Numero(5), Atomo('x'))), Numero(6)))), Numero(0))
    assert(n.limpiar(expr) == Prod(Suma(Prod(Expo(Atomo('x'),Numero(2.0)),Numero(2.0)),Resta(Atomo('x'),Numero(1.0))),Suma(Expo(Atomo('x'),Numero(3.0)),Resta(Suma(Prod(Numero(4.0),Expo(Atomo('x'),Numero(2.0))),Prod(Numero(5.0),Atomo('x'))),Numero(6.0)))))
  }
  test("Test Limpiar 2"){
    val expr = Resta(Prod(Suma(Prod(Expo(Atomo('x'), Numero(4)), Numero(2)), Resta(Expo(Atomo('x'), Numero(3)), Resta(Prod(Atomo('x'), Numero(3)), Numero(6)))), Resta(Expo(Atomo('x'), Numero(5)), Suma(Suma(Suma(Prod(Numero(5), Expo(Atomo('x'), Numero(4))), Prod(Numero(10), Expo(Atomo('x'), Numero(3)))), Prod(Numero(10), Expo(Atomo('x'), Numero(2)))), Resta(Prod(Numero(5), Atomo('x')), Numero(1))))), Numero(0))
    assert(n.limpiar(expr) == Prod(Suma(Prod(Expo(Atomo('x'),Numero(4.0)),Numero(2.0)),Resta(Expo(Atomo('x'),Numero(3.0)),Resta(Prod(Atomo('x'),Numero(3.0)),Numero(6.0)))),Resta(Expo(Atomo('x'),Numero(5.0)),Suma(Suma(Suma(Prod(Numero(5.0),Expo(Atomo('x'),Numero(4.0))),Prod(Numero(10.0),Expo(Atomo('x'),Numero(3.0)))),Prod(Numero(10.0),Expo(Atomo('x'),Numero(2.0)))),Resta(Prod(Numero(5.0),Atomo('x')),Numero(1.0))))))
  }

  test("Test Limpiar 3"){
    val expr = Prod(Suma(Prod(Expo(Atomo('x'), Numero(3)), Numero(-2)), Suma(Expo(Atomo('x'), Numero(2)), Prod(Numero(3), Atomo('x')))), Suma(Prod(Expo(Atomo('x'), Numero(4)), Numero(4)), Suma(Prod(Numero(-5), Expo(Atomo('x'), Numero(2))), Suma(Prod(Numero(6), Atomo('x')), Numero(-7)))))
    assert(n.limpiar(expr) == Prod(Suma(Prod(Expo(Atomo('x'),Numero(3.0)),Numero(-2.0)),Suma(Expo(Atomo('x'),Numero(2.0)),Prod(Numero(3.0),Atomo('x')))),Suma(Prod(Expo(Atomo('x'),Numero(4.0)),Numero(4.0)),Suma(Prod(Numero(-5.0),Expo(Atomo('x'),Numero(2.0))),Suma(Prod(Numero(6.0),Atomo('x')),Numero(-7.0))))))
  }
  test("Test Limpiar 4"){
    val expr5 = Prod(
      Suma(
        Prod(
          Suma(
            Prod(Numero(0), Atomo('x')),
            Numero(3)
          ),
          Expo(Atomo('x'), Numero(4))
        ),
        Suma(
          Prod(Numero(1), Expo(Atomo('x'), Numero(2))),
          Numero(0)
        )
      ),
      Div(
        Logaritmo(
          Suma(
            Prod(Numero(1), Expo(Atomo('x'), Numero(3))),
            Suma(
              Prod(Numero(2), Atomo('x')),
              Numero(1)
            )
          )
        ),
        Suma(
          Prod(Numero(6), Atomo('x')),
          Numero(0)
        )
      )
    )
    assert(n.limpiar(expr5) == Prod(Suma(Prod(Numero(3.0),Expo(Atomo('x'),Numero(4.0))),Expo(Atomo('x'),Numero(2.0))),Div(Logaritmo(Suma(Expo(Atomo('x'),Numero(3.0)),Suma(Prod(Numero(2.0),Atomo('x')),Numero(1.0)))),Prod(Numero(6.0),Atomo('x')))))
  }

  test("Test Limpiar 5") {
    val expr = Resta(
      Prod(
        Suma(
          Prod(Expo(Atomo('x'), Numero(0)), Numero(2)),
          Resta(Expo(Atomo('x'), Numero(0)), Resta(Prod(Atomo('x'), Numero(3)), Numero(6)))
        ),
        Resta(
          Expo(Atomo('x'), Numero(5)),
          Suma(
            Suma(
              Suma(Prod(Numero(5), Expo(Atomo('x'), Numero(4))), Prod(Numero(10), Expo(Atomo('x'), Numero(3)))),
              Prod(Numero(1), Expo(Atomo('x'), Numero(2)))
            ),
            Resta(Prod(Numero(1), Atomo('x')), Numero(1))
          )
        )
      ),
      Numero(0)
    )
    assert(n.limpiar(expr) == Prod(Suma(Numero(2.0),Resta(Numero(1.0),Resta(Prod(Atomo('x'),Numero(3.0)),Numero(6.0)))),Resta(Expo(Atomo('x'),Numero(5.0)),Suma(Suma(Suma(Prod(Numero(5.0),Expo(Atomo('x'),Numero(4.0))),Prod(Numero(10.0),Expo(Atomo('x'),Numero(3.0)))),Expo(Atomo('x'),Numero(2.0))),Resta(Atomo('x'),Numero(1.0))))))

  }

}
