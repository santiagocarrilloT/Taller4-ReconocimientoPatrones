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
    case Resta(Numero(0), e) => limpiar(e)
    case Resta(e, Numero(0)) => limpiar(e)
    case Resta(e1, e2) => Resta(limpiar(e1), limpiar(e2))
    case Resta(e1, e2) =>
      val le1 = limpiar(e1)
      val le2 = limpiar(e2)
      (le1, le2) match {
        case (Numero(0), _) => le2
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
        case (Numero(0), _) => le2
        case (_, Numero(0)) => le1
        case _ => Prod(le1, le2)
      }
    case Div(e, Numero(1)) => limpiar(e)
    case Div(Numero(0), _) => Numero(0)
    case Div(_, Numero(0)) => throw new IllegalArgumentException("Division by zero")
    case Div(e1, e2) => Div(limpiar(e1), limpiar(e2))
    case Div(e1, e2) =>
      val le1 = limpiar(e1)
      val le2 = limpiar(e2)
      (le1, le2) match {
        case (Numero(0), _) => le2
        case (_, Numero(0)) => le1
        case _ => Div(le1, le2)
      }
    case Expo(e1, e2) =>
      val le1 = limpiar(e1)
      val le2 = limpiar(e2)
      (le1, le2) match {
        case (Numero(0), _) => le2
        case (_, Numero(0)) => le1
        case _ => Expo(le1, le2)
      }
    case Logaritmo(e1) => Logaritmo(limpiar(e1))
    case e=>e
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
    val expr1 = Suma(Atomo('x'), Numero(2))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val expr3 = Suma(expr1, Expo(expr2, Numero(5)))
    val expr4 = Logaritmo(Atomo('x'))
    val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
    val expr6 = Expo(Atomo('x'), Numero(3))

    val n = new Newton
    //print(n.mostrar(n.derivar(Suma(Atomo ('k') , Prod (Numero ( 3.0 ) , Atomo ('x') ) ) , Atomo ('x'))))
    //print(n.mostrar(n.derivar(expr6, Atomo('x'))))

    // ejemplo funcion evaluar
    //println(n.evaluar( Logaritmo ( expr1 ) ,Atomo ('x' ) , 5.0 ))
    println(n.limpiar(n.derivar(Suma (Atomo ('k') , Prod(Numero ( 3.0 ) , Atomo ('x'))),Atomo('x'))))
    println(n.mostrar(n.limpiar(n.derivar(Suma(Atomo ('n'),Prod(Numero(3.0),Atomo('x'))),Atomo('x')))))
    println(n.mostrar(Expo(Logaritmo(Atomo('x')), Numero(2))))

//    val e1= Resta ( Prod (Atomo ('x') ,Atomo ('x') ) , Numero ( 2.0 ) )
//    val e2= Resta(Prod(Atomo('x') ,Atomo ('x') ) , Numero ( 4.0 ) )
//    val e3 = Suma(Resta(Prod(Atomo('x') ,Atomo ('x') ) , Numero (4.0)),Prod(Numero (3.0) ,Atomo ('x')))
//    println(n.raizNewton(e1,Atomo('x'),2.0,n.buenaAprox))
//    println(n.raizNewton(e2,Atomo('x'),2.0,n.buenaAprox))
//    println(n.raizNewton(e3,Atomo('x'),2.0,n.buenaAprox))
    println(n.mostrar(Suma(Atomo('x') , Numero(2))))
    println(n.evaluar(Suma(Prod(Numero(2), Atomo('x')), Expo(Numero(3), Numero(2))), Atomo('x'), 4))

  }


}

