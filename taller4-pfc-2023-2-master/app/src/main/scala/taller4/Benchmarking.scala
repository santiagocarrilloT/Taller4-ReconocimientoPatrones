/**
 * Taller 3 - Programación Funcional
 * Autores: Edwar Forero, Santiago Carrillo, Alexis Solis
 * Profesor: Carlos A Delgado
 */
package taller4

import org.scalameter._

class Benchmarking{
  val newtonPar = new NewtonParalela
  val newton = new Newton

  def limpiarVsLimpiarParalela(expr: Expr): Unit = {
    println("---Limpiar vs LimpiarParalela--")
    val timeFinal = new Array[Double](7)
    for (h <- timeFinal.indices) {
      val time = withWarmer(new Warmer.Default) measure {
        newton.limpiar(expr)
      }
      timeFinal(h) = time.value
    }
    println("NewtonSeq " + timeFinal.sum / 7 + "\n")
    for (prof <- 1 to 5) {
      val timeFinal1 = new Array[Double](7)
      for (i <- timeFinal.indices) {
        val time = withWarmer(new Warmer.Default) measure {
          newtonPar.limpiarParalela(expr, 0, prof)
        }
        timeFinal1(i) = time.value
      }
      println("NewtonPar " + timeFinal1.sum / 7 + " Profundidad:" + prof)
    }
    println("\n")
  }

  def derivarVsDerivarParalela(expr: Expr): Unit = {
    println("---Derivar vs DerivarParalela---")
    val timeFinal2 = new Array[Double](7)
    for (h <- timeFinal2.indices) {
      val time = withWarmer(new Warmer.Default) measure {
        newton.derivar(expr, Atomo('x'))
      }
      timeFinal2(h) = time.value
    }
    println("NewtonSeq " + timeFinal2.sum / 7 + "\n")
    for (prof <- 1 to 5) {
      val timeFinal3 = new Array[Double](7)
      for (i <- timeFinal3.indices) {
        val time = withWarmer(new Warmer.Default) measure {
          newtonPar.derivarParalela(expr, Atomo('x'), 0, prof)
        }
        timeFinal3(i) = time.value
      }
      println("NewtonPar " + timeFinal3.sum / 7 + " Profundidad:" + prof)
    }
    println("\n")
  }

  def evaluarVsEvaluarParalela(expr: Expr, v:Double): Unit = {
    println("---Evaluar vs EvaluarParalela---")
    val timeFinal3 = new Array[Double](7)
    for (h <- timeFinal3.indices) {
      val time = withWarmer(new Warmer.Default) measure {
        newton.evaluar(expr, Atomo('x'), v)
      }
      timeFinal3(h) = time.value
    }
    println("NewtonSeq " + timeFinal3.sum / 7 + "\n")
    for (prof <- 1 to 5) {
      val timeFinal5 = new Array[Double](7)
      for (i <- timeFinal5.indices) {
        val time = withWarmer(new Warmer.Default) measure {
          newtonPar.evaluarParalela(expr, Atomo('x'), v, 0,prof)
        }
        timeFinal5(i) = time.value
      }
      println("NewtonPar " + timeFinal5.sum / 7 + " Profundidad:" + prof)
    }
    println("\n")
  }
}
