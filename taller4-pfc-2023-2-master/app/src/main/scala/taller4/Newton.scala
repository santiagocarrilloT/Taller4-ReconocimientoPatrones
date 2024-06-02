/**
 * Taller 3 - Programación Funcional
 * Autores: Edwar Forero, Santiago Carrillo, Alexis Solis
 * Profesor: Carlos A Delgado
 */
package taller4

import scala.annotation.tailrec

trait Expr
case class Numero ( d : Double ) extends Expr
case class Atomo ( x : Char ) extends Expr
case class Suma ( e1 : Expr , e2 : Expr ) extends Expr
case class Prod ( e1 : Expr , e2 : Expr ) extends Expr
case class Resta ( e1 : Expr , e2 : Expr ) extends Expr
case class Div ( e1 : Expr , e2 : Expr ) extends Expr
case class Expo ( e1 : Expr , e2 : Expr ) extends Expr
case class Logaritmo ( e1 : Expr ) extends Expr

class Newton {
  def mostrar(e: Expr): String = e match {
    case Numero(d) => d.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
    case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
    case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
    case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
    case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
    case Logaritmo(e1) => s"(lg(${mostrar(e1)}))"
  }

  def derivar(f: Expr, a: Atomo): Expr = f match {
    case Numero(_) => Numero(0)
    case Atomo(x) => if (x == a.x) Numero(1) else Numero(0)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Prod(e2, e2))
    case Expo(e1, Numero(n)) => Prod(Numero(n), Prod(Expo(e1, Numero(n - 1)), derivar(e1, a)))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
    case Expo(e1, e2) => Prod(Expo(e1, Resta(e2, Numero(1))), Suma(Prod(derivar(e1, a), e2), Prod(e1, Prod(derivar(e2, a), Logaritmo(e1)))))
  }

  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => d
    case Atomo(x) => if (x == a.x) v else throw new IllegalArgumentException(s"no se conoce la variable $x")
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => math.log(evaluar(e1, a, v))
  }

  def limpiar(f: Expr): Expr = f match {
    case Suma(Numero(0), e) => limpiar(e)
    case Suma(e, Numero(0)) => limpiar(e)
    case Suma(e1, e2) =>
      val le1 = limpiar(e1)
      val le2 = limpiar(e2)
      (le1, le2) match {
        case (Numero(0), _) => le2
        case (_, Numero(0)) => le1
        case _ => Suma(le1, le2)
      }
    case Resta(e, Numero(0)) => limpiar(e)
    case Resta(Numero(0), e) => Resta(Numero(0), limpiar(e))
    case Resta(e1, e2) =>
      val le1 = limpiar(e1)
      val le2 = limpiar(e2)
      (le1, le2) match {
        case (Numero(0), _) => Resta(Numero(0), le2)
        case (_, Numero(0)) => le1
        case _ => Resta(le1, le2)
      }
    case Prod(Numero(1), e) => limpiar(e)
    case Prod(e, Numero(1)) => limpiar(e)
    case Prod(Numero(0), _) => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Prod(e1, e2) =>
      val le1 = limpiar(e1)
      val le2 = limpiar(e2)
      (le1, le2) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(0)) => Numero(0)
        case (Numero(1), _) => le2
        case (_, Numero(1)) => le1
        case _ => Prod(le1, le2)
      }
    case Div(e, Numero(1)) => limpiar(e)
    case Div(Numero(0), _) => Numero(0)
    case Div(_, Numero(0)) => throw new IllegalArgumentException("Division by zero")
    case Div(e1, e2) =>
      val le1 = limpiar(e1)
      val le2 = limpiar(e2)
      (le1, le2) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(1)) => le1
        case _ => Div(le1, le2)
      }
    case Expo(e1, Numero(0)) => Numero(1)
    case Expo(e1, Numero(1)) => limpiar(e1)
    case Expo(Numero(0), _) => Numero(0)
    case Expo(Numero(1), _) => Numero(1)
    case Expo(e1, e2) =>
      val le1 = limpiar(e1)
      val le2 = limpiar(e2)
      Expo(le1, le2)
    case Logaritmo(e1) => Logaritmo(limpiar(e1))
    case e => e
  }

  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    @tailrec
    def newtonIter(xi: Double): Double = {
      if (ba(f, a, xi)) xi
      else {
        val fx = evaluar(f, a, xi)
        val fpx = evaluar(derivar(f, a), a, xi)
        newtonIter(xi - fx / fpx)
      }
    }
    newtonIter(x0)
  }

  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    math.abs(evaluar(f, a, d)) < 0.001
  }
}

object Newton {
  def main(args: Array[String]): Unit = {
    val n = new Newton()
    val expr1 = Resta(
      Prod(
        Suma(
          Suma(
            Suma(
              Suma(
                Suma(
                  Suma(
                    Suma(
                      Expo(Atomo('x'), Numero(8)),
                      Prod(Numero(3), Expo(Atomo('x'), Numero(7)))
                    ),
                    Prod(Numero(-5), Expo(Atomo('x'), Numero(6)))
                  ),
                  Prod(Numero(8), Expo(Atomo('x'), Numero(5)))
                ),
                Prod(Numero(-10), Expo(Atomo('x'), Numero(4)))
              ),
              Prod(Numero(12), Expo(Atomo('x'), Numero(3)))
            ),
            Prod(Numero(-15), Expo(Atomo('x'), Numero(2)))
          ),
          Prod(Numero(17), Atomo('x'))
        ),
        Numero(-20)
      ),
      Prod(
        Suma(
          Suma(
            Suma(
              Suma(
                Suma(
                  Suma(
                    Suma(
                      Expo(Atomo('x'), Numero(7)),
                      Prod(Numero(-4), Expo(Atomo('x'), Numero(6)))
                    ),
                    Prod(Numero(6), Expo(Atomo('x'), Numero(5)))
                  ),
                  Prod(Numero(-4), Expo(Atomo('x'), Numero(4)))
                ),
                Prod(Numero(7), Expo(Atomo('x'), Numero(3)))
              ),
              Prod(Numero(-8), Expo(Atomo('x'), Numero(2)))
            ),
            Prod(Numero(9), Atomo('x'))
          ),
          Numero(-10)
        ),
        Prod(
          Resta(
            Prod(
              Suma(
                Suma(
                  Suma(
                    Suma(
                      Suma(
                        Suma(
                          Suma(
                            Suma(
                              Expo(Atomo('x'), Numero(9)),
                              Prod(Numero(-9), Expo(Atomo('x'), Numero(8)))
                            ),
                            Prod(Numero(36), Expo(Atomo('x'), Numero(7)))
                          ),
                          Prod(Numero(-84), Expo(Atomo('x'), Numero(6)))
                        ),
                        Prod(Numero(126), Expo(Atomo('x'), Numero(5)))
                      ),
                      Prod(Numero(-126), Expo(Atomo('x'), Numero(4)))
                    ),
                    Prod(Numero(84), Expo(Atomo('x'), Numero(3)))
                  ),
                  Prod(Numero(-36), Expo(Atomo('x'), Numero(2)))
                ),
                Prod(Numero(9), Atomo('x'))
              ),
              Numero(-1)
            ),
            Prod(
              Suma(
                Suma(
                  Suma(
                    Suma(
                      Suma(
                        Prod(Expo(Atomo('x'), Numero(6)), Numero(-6)),
                        Prod(Numero(15), Expo(Atomo('x'), Numero(4)))
                      ),
                      Resta(
                        Suma(
                          Prod(Numero(20), Expo(Atomo('x'), Numero(3))),
                          Prod(Numero(15), Expo(Atomo('x'), Numero(2)))
                        ),
                        Prod(Numero(6), Atomo('x'))
                      )
                    ),
                    Prod(Numero(-6), Atomo('x'))
                  ),
                  Prod(Numero(1), Atomo('x'))
                ),
                Numero(-10)
              ),
              Suma(
                Suma(
                  Suma(
                    Suma(
                      Suma(
                        Suma(
                          Suma(
                            Prod(Numero(-9), Expo(Atomo('x'), Numero(8))),
                            Prod(Numero(36), Expo(Atomo('x'), Numero(7)))
                          ),
                          Prod(Numero(-84), Expo(Atomo('x'), Numero(6)))
                        ),
                        Prod(Numero(126), Expo(Atomo('x'), Numero(5)))
                      ),
                      Prod(Numero(-126), Expo(Atomo('x'), Numero(4)))
                    ),
                    Prod(Numero(84), Expo(Atomo('x'), Numero(3)))
                  ),
                  Prod(Numero(-36), Expo(Atomo('x'), Numero(2)))
                ),
                Prod(Numero(9), Atomo('x'))
              )
            )
          ),
          Numero(-10)
        )
      )
    )

    println(n.raizNewton(expr1, Atomo('x'), 3.0, n.buenaAprox))
  }
}


