package co.com.scalatraining.modelling.dominio.Cotizaciones

case class Persona(nombre:String) {
  def mayus() = this.nombre.toUpperCase
}

case class CrearCotizacion(empleado: Persona, periodo: String, aportante: String, dias: Int, ibc:Int, salario:Int)

object Classes {

  def limpiarHistorial(lista:List[CrearCotizacion], p:Persona):Int = {
    val r1 = lista.filter(x => x.ibc != 0 && x.dias != 0)
    val r2 = r1.map(x => CrearCotizacion(p, x.periodo, x.aportante, x.dias, x.ibc, 30*x.ibc/x.dias))
    val r3 = r2.distinct
    val rAportante = r3.groupBy(x => x.aportante)
    val r4 = rAportante.map(x => x._1 -> x._2.foldLeft(0){(acumulado, i) =>
      if(acumulado < i.salario){
        i.salario
      }  else {
        acumulado
      }
    })
    r4.foldLeft(0){(ac, item) => ac + item._2}
  }

  def main(args: Array[String]) {
    val p = Persona("Santiago Giraldo Mejia")
    println("-----Limpiar Historial Laboral-----\n")
    val emple = p.mayus()
    println("Empleado:  " + emple + "\n")

    val c1 = CrearCotizacion(p,"2018/07", "S4", 10, 1000000, 0)
    val c2 = CrearCotizacion(p,"2018/07", "S4N", 20, 1000000, 0)
    val c3 = CrearCotizacion(p,"2018/08", "S4N", 30, 2000000, 0)

    println("-----------Cotizaciones------------\n")
    println(c1.periodo + "|   " + c1.aportante + " |   " + c1.dias + "|    " + c1.ibc)
    println(c2.periodo + "|   " + c2.aportante + "|   " + c2.dias + "|    " + c2.ibc)
    println(c3.periodo + "|   " + c3.aportante + "|   " + c3.dias + "|    " + c3.ibc)

    val listaCotizaciones = List(c1, c2, c3)
    //listaCotizaciones.foreach(println)

    val historialLimpio = limpiarHistorial(listaCotizaciones, p)

    println("\nAporte: " + historialLimpio)

  }

  // agrupacion por periodo y aportante 
}