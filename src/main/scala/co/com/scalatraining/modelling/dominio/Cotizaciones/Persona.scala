package co.com.scalatraining.modelling.dominio.Cotizaciones

case class Persona(nombre:String) {

  def mayus() = this.nombre.toUpperCase

}

case class CrearCotizacion(empleado: Persona, periodo: String, aportante: String, dias: Int, ibc:Float) {

  def cotizacion() = 1
}

object Classes {
  def main(args: Array[String]) {
    val p = Persona("Santiago Giraldo Mejia")
    println("-----Limpiar Historial Laboral-----\n")
    val emple = p.mayus()
    println("Empleado:  " + emple + "\n")

    val c1 = CrearCotizacion(p,"2018/07", "S4", 10, 1000000)
    val c2 = CrearCotizacion(p,"2018/07", "S4N", 20, 1000000)
    val c3 = CrearCotizacion(p,"2018/08", "S4N", 30, 2000000)

    println("-----------Cotizaciones------------\n")
    println(c1.periodo + "|   " + c1.aportante + " |   " + c1.dias + "|    " + c1.ibc)
    println(c2.periodo + "|   " + c2.aportante + "|   " + c2.dias + "|    " + c2.ibc)
    println(c3.periodo + "|   " + c3.aportante + "|   " + c3.dias + "|    " + c3.ibc)

  }

  // agrupacion por periodo y aportante 
}