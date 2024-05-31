package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class derivarTest extends AnyFunSuiteLike {
  val n = new Newton()

  test("test Derivar 1") {
    val expr = Prod(Atomo('x'), Atomo('y'))
    assert(n.derivar(expr, Atomo('x')) == Suma(Prod(Numero(1), Atomo('y')), Prod(Atomo('x'), Numero(0))))
  }

  test("test Derivar 2"){
    val expr = Div(Atomo('x'), Atomo('y'))
    assert(n.derivar(expr, Atomo('x')) == Div(Resta(Prod(Numero(1),
      Atomo('y')), Prod(Atomo('x'), Numero(0))), Prod(Atomo('y'), Atomo('y'))))
  }

  test("test Derivar 3"){
    assert(n.derivar(Atomo('y'), Atomo('x')) == Numero(0))
  }

  test("test Derivar 4"){
      val expr = Suma(Atomo('x'), Atomo('y'))
      assert(n.derivar(expr, Atomo('x')) == Suma(Numero(1), Numero(0)))
    }

  test("test Derivar 5") {
    val expr = Logaritmo(Atomo('x'))
    assert(n.derivar(expr, Atomo('x')) == Div(Numero(1), Atomo('x')))
  }

}
