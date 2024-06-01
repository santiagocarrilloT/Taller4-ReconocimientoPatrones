package taller4

import common._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
class NewtonParalela {

  def mostrarPar(prof:Int)(maxProf:Int)(e: Expr): String = {
    if (prof >= maxProf){
      val a = new Newton
      a.mostrar(e)
    }else{
      e match {
        case Numero(d) => d.toString
        case Atomo(x) => x.toString
        case Suma(e1, e2) =>
          val (s1, s2) = parallel(mostrarPar(prof+1)(maxProf)(e1), mostrarPar(prof+1)(maxProf)(e2))
          s"($s1 + $s2)"

        case Prod(e1, e2) =>
          val (p1, p2) = parallel(mostrarPar(prof+1)(maxProf)(e1), mostrarPar(prof+1)(maxProf)(e2))
          s"($p1 * $p2)"

        case Resta(e1, e2) =>
          val (r1, r2) = parallel(mostrarPar(prof+1)(maxProf)(e1), mostrarPar(prof+1)(maxProf)(e2))
          s"($r1 - $r2)"

        case Div(e1, e2) =>
          val (d1, d2) = parallel(mostrarPar(prof+1)(maxProf)(e1), mostrarPar(prof+1)(maxProf)(e2))
          s"($d1 / $d2)"

        case Expo(e1, e2) =>
          val (ex1, ex2) = parallel(mostrarPar(prof+1)(maxProf)(e1), mostrarPar(prof+1)(maxProf)(e2))
          s"($ex1 ^ $ex2)"

        case Logaritmo(e1) =>
          val l1 = mostrarPar(prof+1)(maxProf)(e1)
          s"(lg($l1))"
      }

    }
  }

}

//para imprimir

