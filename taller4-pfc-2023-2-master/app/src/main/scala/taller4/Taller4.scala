/**
  * Taller 3 - Programación Funcional
  * Autores: Edwar Forero, Santiago Carrillo, Alexis Solis
  * Profesor: Carlos A Delgado
  */
package taller4

object Taller4{

  def main(args: Array[String]): Unit = {
    val bm = new Benchmarking
    val expr = Suma(
      Prod(
        Suma(
          Prod(Numero(2), Expo(Atomo('x'), Numero(12))),
          Suma(
            Prod(Numero(3), Expo(Atomo('x'), Numero(11))),
            Suma(
              Prod(Numero(4), Expo(Atomo('x'), Numero(10))),
              Suma(
                Prod(Numero(5), Expo(Atomo('x'), Numero(9))),
                Suma(
                  Prod(Numero(6), Expo(Atomo('x'), Numero(8))),
                  Suma(
                    Prod(Numero(7), Expo(Atomo('x'), Numero(7))),
                    Suma(
                      Prod(Numero(8), Expo(Atomo('x'), Numero(6))),
                      Suma(
                        Prod(Numero(9), Expo(Atomo('x'), Numero(5))),
                        Suma(
                          Prod(Numero(10), Expo(Atomo('x'), Numero(4))),
                          Suma(
                            Prod(Numero(11), Expo(Atomo('x'), Numero(3))),
                            Suma(
                              Prod(Numero(12), Expo(Atomo('x'), Numero(2))),
                              Suma(
                                Prod(Numero(13), Atomo('x')),
                                Numero(14)
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
        ),
        Suma(
          Prod(Numero(15), Expo(Atomo('x'), Numero(13))),
          Suma(
            Prod(Numero(16), Expo(Atomo('x'), Numero(14))),
            Suma(
              Prod(Numero(17), Expo(Atomo('x'), Numero(15))),
              Suma(
                Prod(Numero(18), Expo(Atomo('x'), Numero(16))),
                Suma(
                  Prod(Numero(19), Expo(Atomo('x'), Numero(17))),
                  Prod(Numero(20), Expo(Atomo('x'), Numero(18)))
                )
              )
            )
          )
        )
      ),
      Div(
        Expo(
          Suma(
            Prod(Numero(21), Expo(Atomo('x'), Numero(19))),
            Suma(
              Prod(Numero(22), Expo(Atomo('x'), Numero(20))),
              Suma(
                Prod(Numero(23), Expo(Atomo('x'), Numero(21))),
                Suma(
                  Prod(Numero(24), Expo(Atomo('x'), Numero(22))),
                  Suma(
                    Prod(Numero(25), Expo(Atomo('x'), Numero(23))),
                    Numero(26)
                  )
                )
              )
            )
          ),
          Numero(2)
        ),
        Suma(
          Prod(Numero(27), Expo(Atomo('x'), Numero(24))),
          Suma(
            Prod(Numero(28), Expo(Atomo('x'), Numero(25))),
            Numero(29)
          )
        )
      )
    )
    val expr2 = Resta(
      Div(
        Prod(
          Suma(
            Expo(
              Atomo('x'),
              Numero(2)
            ),
            Numero(2)
          ),
          Resta(
            Logaritmo(
              Atomo('x')
            ),
            Numero(3)
          )
        ),
        Atomo('x')
      ),
      Logaritmo(
        Div(
          Expo(
            Atomo('x'),
            Numero(2)
          ),
          Numero(2)
        )
      )
    )
    val expr3 = Suma(
      Prod(
        Resta(
          Expo(
            Suma(
              Logaritmo(
                Atomo('x')
              ),
              Numero(2)
            ),
            Numero(2)
          ),
          Div(
            Atomo('x'),
            Numero(3)
          )
        ),
        Atomo('x')
      ),
      Div(
        Logaritmo(
          Expo(
            Atomo('x'),
            Numero(2)
          )
        ),
        Numero(2)
      )
    )
    println("-----------------------Expr 1--------------------------")
    bm.limpiarVsLimpiarParalela(expr)
    bm.derivarVsDerivarParalela(expr)
    bm.evaluarVsEvaluarParalela(expr, 2.0)
    println("-----------------------Expr 2--------------------------")
    bm.limpiarVsLimpiarParalela(expr2)
    bm.derivarVsDerivarParalela(expr2)
    bm.evaluarVsEvaluarParalela(expr2, 2.0)
    println("-----------------------Expr 3--------------------------")
    bm.limpiarVsLimpiarParalela(expr3)
    bm.derivarVsDerivarParalela(expr3)
    bm.evaluarVsEvaluarParalela(expr3, 4.0)
  }
 }
