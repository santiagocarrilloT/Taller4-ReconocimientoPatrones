package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class raizNewtonTest extends AnyFunSuiteLike {
  val n = new Newton()

  test("testRaizNewton 1") {
    val expr1 = Prod(
      Numero(2),
      Suma(
        Resta(
          Expo(Atomo('x'), Numero(5)),
          Prod(Numero(4), Expo(Atomo('x'), Numero(4)))
        ),
        Suma(
          Prod(Numero(3), Expo(Atomo('x'), Numero(3))),
          Resta(
            Prod(Numero(2), Expo(Atomo('x'), Numero(2))),
            Numero(5)
          )
        )
      )
    )
    assert(n.raizNewton(expr1, Atomo('x'), 1.5, n.buenaAprox) == 2.716369089563468)
  }
  test("testRaizNewton 2") {
    val expr1 = Div(
      Suma(
        Prod(Numero(2), Expo(Atomo('x'), Numero(3))),
        Prod(Numero(3), Expo(Atomo('x'), Numero(2)))
      ),
      Suma(
        Prod(Numero(4), Atomo('x')),
        Numero(5)
      )
    )
    assert(n.raizNewton(expr1, Atomo('x'), -2.0, n.buenaAprox) == -1.4999674349757834)
  }
  test("testRaizNewton 3") {
    val expr1 = Div(
      Suma(
        Prod(Numero(6), Expo(Atomo('x'), Numero(6))),
        Resta(
          Prod(Numero(5), Expo(Atomo('x'), Numero(5))),
          Suma(
            Prod(Numero(4), Expo(Atomo('x'), Numero(4))),
            Numero(3)
          )
        )
      ),
      Suma(
        Prod(Numero(3), Expo(Atomo('x'), Numero(3))),
        Resta(
          Prod(Numero(2), Expo(Atomo('x'), Numero(2))),
          Suma(
            Prod(Numero(2), Atomo('x')),
            Numero(1)
          )
        )
      )
    )
    assert(n.raizNewton(expr1, Atomo('x'), 1.0, n.buenaAprox) == 0.8788566450542122)
  }
  test("testRaizNewton 4") {
    val expr1 = Suma(
      Prod(Numero(2), Expo(Atomo('x'), Numero(5))),
      Suma(
        Prod(Numero(3), Expo(Atomo('x'), Numero(4))),
        Suma(
          Prod(Numero(4), Expo(Atomo('x'), Numero(3))),
          Suma(
            Prod(Numero(5), Expo(Atomo('x'), Numero(2))),
            Suma(
              Prod(Numero(6), Atomo('x')),
              Numero(7)
            )
          )
        )
      )
    )
    assert(n.raizNewton(expr1,Atomo('x'),0.5,n.buenaAprox) == -1.300287313650643)
  }
  test("testRaizNewton 5"){
    val s = Suma(
      Prod(Numero(3), Expo(Atomo('x'), Numero(4))),
      Suma(
        Prod(Numero(2), Expo(Atomo('x'), Numero(3))),
        Suma(
          Prod(Numero(6), Expo(Atomo('x'), Numero(2))),
          Suma(
            Prod(Numero(5), Atomo('x')),
            Suma(
              Numero(7),
              Suma(
                Prod(Numero(8), Expo(Atomo('x'), Numero(5))),
                Suma(
                  Prod(Numero(9), Expo(Atomo('x'), Numero(6))),
                  Suma(
                    Prod(Numero(10), Expo(Atomo('x'), Numero(7))),
                    Suma(
                      Prod(Numero(11), Expo(Atomo('x'), Numero(8))),
                      Suma(
                        Prod(Numero(12), Expo(Atomo('x'), Numero(9))),
                        Suma(
                          Prod(Numero(13), Expo(Atomo('x'), Numero(10))),
                          Suma(
                            Prod(Numero(14), Expo(Atomo('x'), Numero(11))),
                            Suma(
                              Prod(Numero(15), Expo(Atomo('x'), Numero(12))),
                              Suma(
                                Prod(Numero(16), Expo(Atomo('x'), Numero(13))),
                                Suma(
                                  Prod(Numero(17), Expo(Atomo('x'), Numero(14))),
                                  Suma(
                                    Prod(Numero(18), Expo(Atomo('x'), Numero(15))),
                                    Suma(
                                      Prod(Numero(19), Expo(Atomo('x'), Numero(16))),
                                      Prod(Numero(20), Expo(Atomo('x'), Numero(17)))
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    assert(n.raizNewton(s,Atomo('x'),2.0,n.buenaAprox) == -0.9637458601898243)// -0.9637458601898243

  }

}
