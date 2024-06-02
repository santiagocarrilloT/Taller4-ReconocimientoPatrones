/**
 * Taller 3 - Programación Funcional
 * Autores: Edwar Forero, Santiago Carrillo, Alexis Solis
 * Profesor: Carlos A Delgado
 */
package taller4

import common.parallel



class NewtonParalela {
  /**
   * Se usa la paralelización de tareas para realizar las funciones de 'evaluarParalela', 'derivarParalela' y
   * 'limpiarParalela', debido a que al tratarse de operaciones matématicas y recursividad, se espera que
   * exista un mejor resultado en cuanto a tiempo de ejecución con esta técnica. Además, la paralelización
   * de datos se usa para colecciones de datos y posiblemente no daría un rendimiento óptimo.
   * Al momento de la ejecución el tipo de paralelización no mostró resultados de un mejor rendmimiento, comparado a
   * la forma secuencial, esto puede ser a que tal vez la paralelización no se encuentra muy optimizada, para
   * las funciones que se realizaron.
   *
   * @param evaluarParalela se usa la paralelización en cada uno de los casos donde hay una operación que reciba 2 parámetros,
   * de forma en que se evalúan ambos parámetros y se paralelizan las tareas hasta que se alcance un límite total de
   * profundidad y una vez se sobrepase empezará a usar la función secuencial.
   *
   * @param derivarParalela En este caso se busca algo similiar a la anterior función anterior.
   *
   * @param limpiarParalela En este caso se espera realizar un tipo de paralelización para los casos donde se espera que no
   * se realicen operaciones que dependan de otras operaciones, ya que en algunos casos puede llegar a dañar el resultado.
   * En cuanto a la profundidad se usa el mismo criterio que en los anteriorescasos.
   */


  def evaluarParalela(f: Expr, a: Atomo, v: Double, profundidad: Int, maxProfundidad: Int): Double = {
    if(profundidad >= maxProfundidad){
      val n = new Newton
      n.evaluar(f, a, v)
    }else{
      f match {
        case Numero(d) => d
        case Atomo(x) => if (x == a.x) v else throw new IllegalArgumentException(s"no se conoce la variable $x")
        case Suma(e1, e2) => {
          val (v1, v2) = parallel(evaluarParalela(e1, a, v, profundidad + 1, maxProfundidad), evaluarParalela(e2, a, v, profundidad + 1, maxProfundidad))
          v1 + v2
        }
        case Prod(e1, e2) => {
          val (v1, v2) = parallel(evaluarParalela(e1, a, v, profundidad + 1, maxProfundidad), evaluarParalela(e2, a, v, profundidad + 1, maxProfundidad))
          v1 * v2
        }
        case Resta(e1, e2) => {
          val (v1, v2) = parallel(evaluarParalela(e1, a, v, profundidad + 1, maxProfundidad), evaluarParalela(e2, a, v, profundidad + 1, maxProfundidad))
          v1 - v2
        }
        case Div(e1, e2) => {
          val (v1, v2) = parallel(evaluarParalela(e1, a, v, profundidad + 1, maxProfundidad), evaluarParalela(e2, a, v, profundidad + 1, maxProfundidad))
          v1 / v2
        }
        case Expo(e1, e2) => {
          val (v1, v2) = parallel(evaluarParalela(e1, a, v, profundidad + 1, maxProfundidad), evaluarParalela(e2, a, v, profundidad + 1, maxProfundidad))
          math.pow(v1, v2)
        }
        case Logaritmo(e1) => math.log(evaluarParalela(e1, a, v, profundidad + 1, maxProfundidad))
      }
    }
  }

  def derivarParalela( f: Expr, a: Atomo, profundidad: Int, maxProfundidad: Int): Expr = {
    if(profundidad >= maxProfundidad){
      val n = new Newton
      n.derivar(f, a)
    }else{
      f match {
        case Numero(_) => Numero(0)
        case Atomo(x) => if (x == a.x) Numero(1) else Numero(0)
        case Suma(e1, e2) => {
          val (der1, der2) = parallel(derivarParalela(e1, a, profundidad + 1, maxProfundidad), derivarParalela(e2, a, profundidad + 1, maxProfundidad))
          Suma(der1, der2)
        }
        case Resta(e1, e2) => {
          val (der1, der2) = parallel(derivarParalela(e1, a, profundidad + 1, maxProfundidad), derivarParalela(e2, a, profundidad + 1, maxProfundidad))
          Resta(der1, der2)
        }
        case Prod(e1, e2) => {
          val (der1, der2) = parallel(derivarParalela(e1, a, profundidad + 1, maxProfundidad), derivarParalela(e2, a, profundidad + 1, maxProfundidad))
          Suma(Prod(der1, e2), Prod(e1, der2))
        }
        case Div(e1, e2) => {
          val (der1, der2) = parallel(derivarParalela(e1, a, profundidad + 1, maxProfundidad), derivarParalela(e2, a, profundidad + 1, maxProfundidad))
          Div(Resta(Prod(der1, e2), Prod(e1, der2)), Prod(e2, e2))
        }
        case Expo(e1, Numero(n)) => {
          val der1 = derivarParalela(e1, a, profundidad + 1, maxProfundidad)
          Prod(Numero(n), Prod(Expo(e1, Numero(n - 1)), der1))
        }
        case Logaritmo(e1) => Div(derivarParalela(e1, a, profundidad + 1, maxProfundidad), e1)
        case Expo(e1, e2 ) => {
          val (der1, der2) = parallel(derivarParalela(e1, a, profundidad + 1, maxProfundidad), derivarParalela(e2, a, profundidad + 1, maxProfundidad))
          Prod(Expo(e1, Resta(e2, Numero(1))), Suma(Prod(der1, e2), Prod(e1, Prod(der2, Logaritmo(e1)))))
        }
      }
    }

  }

  def limpiarParalela(f: Expr, profundidad: Int, maxProfundidad: Int): Expr = {
    if (profundidad >= maxProfundidad) {
      val n = new Newton
      n.limpiar(f)
    } else {
      f match {
        case Suma(Numero(0), e) => limpiarParalela(e, profundidad + 1, maxProfundidad)
        case Suma(e, Numero(0)) => limpiarParalela(e, profundidad + 1, maxProfundidad)
        case Suma(e1, e2) =>
          val le1 = parallel(limpiarParalela(e1, profundidad + 1, maxProfundidad),limpiarParalela(e2, profundidad + 1, maxProfundidad))
          (le1) match {
            case (Numero(0), _) => le1._2
            case (_, Numero(0)) => le1._1
            case _ => Suma(le1._1, le1._2)
          }
        case Resta(e, Numero(0)) => limpiarParalela(e, profundidad + 1, maxProfundidad)
        case Resta(Numero(0), e) => Resta(Numero(0), limpiarParalela(e, profundidad + 1, maxProfundidad))
        case Resta(e1, e2) =>
          val le1 = parallel(limpiarParalela(e1, profundidad + 1, maxProfundidad),limpiarParalela(e2, profundidad + 1, maxProfundidad))
          (le1) match {
            case (Numero(0), _) => Resta(Numero(0), le1._2)
            case (_, Numero(0)) => le1._1
            case _ => Resta(le1._1, le1._2)
          }
        case Prod(Numero(1), e) => limpiarParalela(e, profundidad + 1, maxProfundidad)
        case Prod(e, Numero(1)) => limpiarParalela(e, profundidad + 1, maxProfundidad)
        case Prod(Numero(0), _) => Numero(0)
        case Prod(_, Numero(0)) => Numero(0)
        case Prod(e1, e2) =>
          val le1 = parallel(limpiarParalela(e1, profundidad + 1, maxProfundidad),limpiarParalela(e2, profundidad + 1, maxProfundidad))
          (le1) match {
            case (Numero(0), _) => Numero(0)
            case (_, Numero(0)) => Numero(0)
            case (Numero(1), _) => le1._2
            case (_, Numero(1)) => le1._1
            case _ => Prod(le1._1, le1._2)
          }
        case Div(e, Numero(1)) => limpiarParalela(e, profundidad + 1, maxProfundidad)
        case Div(Numero(0), _) => Numero(0)
        case Div(_, Numero(0)) => throw new IllegalArgumentException("Division by zero")
        case Div(e1, e2) =>
          val le1 = parallel(limpiarParalela(e1, profundidad + 1, maxProfundidad),limpiarParalela(e2, profundidad + 1, maxProfundidad))
          (le1) match {
            case (Numero(0), _) => Numero(0)
            case (_, Numero(1)) => le1._1
            case _ => Div(le1._1, le1._2)
          }
        case Expo(e1, Numero(0)) => Numero(1)
        case Expo(e1, Numero(1)) => limpiarParalela(e1, profundidad + 1, maxProfundidad)
        case Expo(Numero(0), _) => Numero(0)
        case Expo(Numero(1), _) => Numero(1)
        case Expo(e1, e2) =>
          val le1 = parallel(limpiarParalela(e1, profundidad + 1, maxProfundidad),limpiarParalela(e2, profundidad + 1, maxProfundidad))
          Expo(le1._1, le1._2)
        case Logaritmo(e1) => Logaritmo(limpiarParalela(e1, profundidad + 1, maxProfundidad))
        case e => e
      }
    }
  }

  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    math.abs(evaluarParalela(f,a,d,0,2)) < 0.001
  }
}