package taller4

class NewtonParalela {

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  def evaluarPar(f: Expr, a: Atomo, v: Double): Future[Double] = f match {
    case Numero(d) => Future.successful(d)
    case Atomo(x) => Future.successful(if (x == a.x) v else throw new IllegalArgumentException(s"Unknown variable $x"))
    case Suma(e1, e2) => for {
      v1 <- evaluarPar(e1, a, v)
      v2 <- evaluarPar(e2, a, v)
    } yield v1 + v2
    case Prod(e1, e2) => for {
      v1 <- evaluarPar(e1, a, v)
      v2 <- evaluarPar(e2, a, v)
    } yield v1 * v2
    case Resta(e1, e2) => for {
      v1 <- evaluarPar(e1, a, v)
      v2 <- evaluarPar(e2, a, v)
    } yield v1 - v2
    case Div(e1, e2) => for {
      v1 <- evaluarPar(e1, a, v)
      v2 <- evaluarPar(e2, a, v)
    } yield v1 / v2
    case Expo(e1, e2) => for {
      v1 <- evaluarPar(e1, a, v)
      v2 <- evaluarPar(e2, a, v)
    } yield math.pow(v1, v2)
    case Logaritmo(e1) => for {
      v1 <- evaluarPar(e1, a, v)
    } yield math.log(v1)
  }


}
