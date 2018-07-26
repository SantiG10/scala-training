package co.com.scalatraining.effects

import org.scalatest.FunSuite

import scala.collection.immutable.Seq

class OptionSuite extends FunSuite {

  test("Se debe poder crear un Option con valor"){
    val s = Option{
      1
    }
    assert(s === Some(1))
  }

  test("Se debe poder crear un Option(some) con valor"){
    val s = Some{
      1
    }
    assert(s === Some(1))
  }

  test("Se debe poder crear un Option(some) con null"){
    val s = Some{
      null
    }
    assert(s === Some(null))
  }

  test("Se debe poder crear un Option con null"){
    val s = Option{
      null
    }
    assert(s === None)
  }

  test("Se debe poder crear un Option para denotar que no hay valor"){
    val s = None
    assert(s === None)
  }

  test("Es inseguro acceder al valor de un Option con get"){
    val s = None
    assertThrows[NoSuchElementException]{
      val r = s.get
    }
  }

  test("Se debe poder hacer pattern match sobre un Option") {
    val lista: Seq[Option[String]] = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre: Option[String] = lista(1)
    var res = ""
    res = nombre match {
      case Some(nom) => nom
      case None => "NONAME"
    }
    assert(res == "NONAME")
  }

  test("Fold en Option"){
    val o = Option(1)

    val res: Int = o.fold{
      10  // cuando es None
    }{
      x => x + 20  // cuando es Some
    }

    assert(res == 21)
  }

  /*test("Fold en Option de null"){
    val o: Option[Int] = Option(null)

    val res: Int = o.fold{
      if (x % 2 == 0){

      }else{

      }
      10  // cuando es None
    }{
      x => x + 20  // cuando es Some
    }

    assert(res == 10)
  }*/

  test("Se debe poder saber si un Option tiene valor con isDefined") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)
    assert(nombre.isDefined)
  }

  test("Se debe poder acceder al valor de un Option de forma segura con fold") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(1)
    val res = nombre.fold{
      "NONAME"
    }{
      x => x
    }
    assert(res == "NONAME")
  }

  test("Se debe poder acceder al valor de un Option de forma segura con getOrElse") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(1)
    val res = nombre.getOrElse("NONAME")
    assert(res == "NONAME")
  }

  test("Un Option se debe poder transformar con un map") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)
    val nombreCompleto: Option[String] = nombre.map(s => s + " Felipe")
    assert(nombreCompleto.getOrElse("NONAME") == "Andres Felipe")
  }

  test("Un Option se debe poder transformar con flatMap en otro Option") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)

    val resultado: Option[String] = nombre.flatMap(s => Option(s.toUpperCase))
    resultado.map( s => assert( s == "ANDRES"))
  }

  test("Un Option se debe poder filtrar con una hof con filter") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val option0 = lista(0)
    val option1 = lista(1)
    val res0 = option0.filter(_>10)
    val res1 = option1.filter(_>10)
    val res2 = option0.filter(_>4)

    assert(res0 == None)
    assert(res1 == None)
    assert(res2 == Some(5))
  }

  test("for comprehensions en Option") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val s1 = lista(0)
    val s2 = lista(2)

    val resultado = for {
      x <- s1
      y <- s2
    } yield x+y

    assert(resultado == Some(45))
  }

  test("for comprehensions en Option2") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val s1 = lista(0)
    val s2 = lista(2)
    val s3 = Some(5)

    val resultado = for {
      x <- s1
      y <- s2
      z <- s3
    } yield x+y+z

    assert(resultado == Some(50))
  }

  test("for comprehensions en Option con flatMap") {
    val o1 = Some(40)
    val o2 = Some(5)
    val o3 = Some(5)

    // es lo mismo que hacer el for comprehensions

    val resultado = o1.flatMap{ x =>
      o2.flatMap{ y =>
        o3.flatMap{ z =>
          Option(x+y+z)
        }
      }
    }

    assert(resultado == Some(50))
  }

  test("for comprehensions en Option3") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val s1 = lista(0)
    val s2 = lista(2)
    val s3 = Some(5)

    def foo(x:Int): Some[Int] = {
      println(s"Ejecutando con fu $x")
      Some(x)
    }
    def bar(x:Int): Option[Int] = {
      println(s"Ejecutando con bar $x")
      None
    }

    val resultado = for {
      x <- foo(1)
      a <- foo(1)
      b <- foo(1)
      c <- foo(1)
      d <- foo(1)
      e <- foo(1)
      f <- foo(1)
      g <- foo(1)
      y <- bar(2)
      z <- foo(3)
    } yield x+y+z

    assert(resultado == None)

  }

  test("for comprehesions None en Option") {
    val consultarNombre = Some("Andres")
    val consultarApellido = Some("Estrada")
    val consultarEdad = None
    val consultarSexo = Some("M")

    val resultado = for {
      nom <- consultarNombre
      ape <- consultarApellido
      eda <- consultarEdad
      sex <- consultarSexo
    //} yield (nom+","+ape+","+eda+","+sex)
    } yield (s"$nom $ape, $eda,$sex")

    assert(resultado == None)
  }

  test("for comprehesions None en Option 2") {

    def consultarNombre(dni:String): Option[String] = Some("Felix")
    def consultarApellido(dni:String): Option[String] = Some("Vergara")
    def consultarEdad(dni:String): Option[String] = None
    def consultarSexo(dni:String): Option[String] = Some("M")

    val dni = "8027133"
    val resultado = for {
      nom <- consultarNombre(dni)
      ape <- consultarApellido(dni)
      eda <- consultarEdad(dni)
      sex <- consultarSexo(dni)
    //} yield (nom+","+ape+","+eda+","+sex)
    } yield (s"$nom $ape, $eda,$sex")

    assert(resultado == None)
  }

  test("Cuando un pattern match es igual que hacer flatMap"){
    val nombre: Option[String] = Some("Santiago")
    var res1: Option[String] = Some("")
    var res2: Option[String] = Some("")

    def foo(x:String): Some[String] = Some(x)

    res1 = nombre match {
      case None => None
      case Some(x) => foo(x)
    }

    res2 = nombre.flatMap(foo(_))

    assert(res1 == res2)
  }

  test("Cuando un pattern match es igual que hacer flatten"){
    val nombre: Option[Option[String]] = Some(Some("Santiago"))
    var res1: Option[String] = Some("")
    var res2: Option[String] = Some("")

    res1 = nombre match {
      case None => Some("NONAME")
      case Some(x) => x
    }

    res2 = nombre.flatten

    assert(res1 == res2)
  }

  test("Cuando un pattern match es igual que hacer map"){
    val valor: Option[Int] = Some(1)

    def foo(x:Int): Option[Int] = Some(x + 1)

    val res1: Option[Option[Int]] = valor match {
      case None => None
      case Some(x) => Some(foo(x))
    }

    val res2: Option[Option[Int]] = valor.map(foo(_))

    assert(res1 == res2)
  }

  test("Cuando un pattern match es igual que hacer foreach"){
    val valor = Some(1)

    var i: Int = 0
    valor.foreach(x => i += 1)

    assert(1 == i)
  }

  test("Cuando un pattern match es igual que hacer isDefined"){
    val valor: Option[Int] = Some(1)

    val res1 = valor match {
      case None => false
      case Some(_) => true
    }

    val res2 = valor.isDefined

    assert(res1 == res2)
  }

  test("Cuando un pattern match es igual que hacer isEmpty"){
    val valor: Option[Int] = Some(1)

    val res1 = valor match {
      case None => true
      case Some(_) => false
    }

    val res2 = valor.isEmpty

    assert(res1 == res2)
  }

  /*test("Cuando un pattern match es igual que hacer forall"){
    val valor: Option[Int] = Some(2)

    def foo(x:Option[Int]): Boolean = x.forall(x => x < 5)

    val res1 = valor match {
      case None => true
      case Some(x) => foo(Some(x))
    }

    //val res2 = valor.forall(foo(Some(_)))

    assert(res1 == res2)
  }*/

}

