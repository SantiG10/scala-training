package co.com.scalatraining.collections

import org.scalatest.FunSuite

import scala.collection.immutable.Seq

/**
  * Hasta Scala 2.12 la taxonomía de colecciones es la siguiente:
  *
  * Para inmutables:
  * https://docs.scala-lang.org/resources/images/collections.immutable.png
  *
  * Para mutables:
  * https://docs.scala-lang.org/resources/images/collections.mutable.png
  *
  * La documentación de Scala para colecciones la puede encontrar en:
  * https://docs.scala-lang.org/overviews/collections/introduction.html
  *
  * El api doc de colecciones lo encuentra en:
  *
  * https://www.scala-lang.org/api/current/scala/collection/index.html
  */
class ListSuite extends FunSuite {

  test("Una List se debe poder construir") {

    val lista: List[Int] = List(1, 2, 3, 4)
    val lista2 = 1 :: 2 :: 3 :: 4 :: Nil
    assert(lista == lista2)
  }

  test("Se puede hacer una lista de un solo tipo"){
    assertDoesNotCompile( "val l = List[String](\"1\", \"2\", 3)")
  }


  test("Una lista se debe poder recorrer imperativamente") {
    val lista = List(1, 2, 3, 4)
    assertResult(10) {
      var sum = 0
      lista.foreach((x) =>
        sum += x
      )
      sum
    }
  }


  test("A una lista se le debe poder adicionar elementos"){
    val l1 = List(1,2,3)
    val l2 = 4::l1
    assert(l2 == List(4,1,2,3))
  }

  test("Se pueden concatenar listas"){
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)

    val l3 = l1 ::: l2
    assert(l3 == List(1,2,3,4,5,6))
  }

  test("Adicionar un elemento con :+"){
    val l1 = List(1,2,3)
    val l2 = l1:+4

    assert(l2 == List(1,2,3,4))
  }

  test("Adicionar un elemento con"){
    val l1 = List(2,3,4)
    val l2 = 1 :: (l1:+5)

    assert(l2 == List(1,2,3,4,5))

    val res = 1+:l1:+5
    assert(res == List(1,2,3,4,5))

  }

  test("A una lista se le debe poder eliminar elementos con drop") {
    val lista = List(1, 2, 3, 4)
    val dropped =lista.drop(2)

    assertResult(List(3, 4)) {
      dropped
    }
  }

  test("A una lista se le pueden descartar elementos en una direccion determinada (right)") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(1, 2)) {
      lista.dropRight(2)
    }
  }

  test("Se debe poder consultar el primer elemento de una lista de forma insegura") {
    val lista = List(1, 2, 3, 4)
    assertResult(1) {
      lista.head
    }
  }

  test("Que pasa si hacemos head a una List()") {
    val lista = List()
    assertThrows[NoSuchElementException] {
      lista.head
    }
  }

  test("Se debe poder obtener todos los elementos de una lista sin el primer elemento") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(2, 3, 4)) {
      lista.tail
    }
  }

  test("Que pasa si hacemos tail a un List()") {
    val lista = List()
    assertThrows[UnsupportedOperationException] {
      val res =  lista.tail
    }
  }

  test("Una lista se debe poder filtrar con una hof") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(2, 4)) {
      lista.filter(numero =>
        numero % 2 == 0
      )

      lista.filter(_%2==0)  // otra forma de hacerlo, pero no es recomendado

    }
  }

  test("Una lista se debe poder eliminar bajo una condicion") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(2, 3, 4)) {
      lista.dropWhile(numero =>  // borra mientras se cumpla
        numero % 2 != 0
      )
    }

    assertResult(List(1, 2, 3, 4)) {
      lista.dropWhile(numero =>  // borra mientras se cumpla como uno no es par no borra ninguno, queda igual.
        numero % 2 == 0
      )
    }
  }

  test("Una lista se debe poder acumular") {
    val lista = List(1, 2, 3, 4, 5)
    assertResult(15) {
      lista.fold(0) { (acumulado, item) =>
        acumulado + item
      }
      lista.sum
    }
  }

  test("Una lista se debe poder multiplicar") {
    val lista = List(1, 2, 3, 4, 5)
    assertResult(120) {
      lista.fold(1) { (acumulado, item) =>
        acumulado * item
      }
      lista.product
    }
  }

  test("Una lista se debe poder acumular en una direccion determinada (izquierda)") {
    val lista = List("Andres", "Felipe", "Juan", "Carlos")
    assertResult("1.Andres,2.Felipe,3.Juan,4.Carlos,") {
      var cont = 0
      lista.foldLeft("") { (resultado, item) =>
        cont = cont + 1
        resultado + cont + "." + item + ","
      }
    }
  }

  test("Una lista se debe poder acumular en una direccion determinada (derecha)") {
    val lista = List("Andres", "Felipe", "Juan", "Carlos")
    assertResult("1.Carlos,2.Juan,3.Felipe,4.Andres,") {
      var cont = 0
      lista.foldRight("") { (item, resultado) =>
        cont = cont + 1
        resultado + cont + "." + item + ","
      }
    }
  }

  test("fold sobre una List de objetos"){
    case class MyCaseClass(i:Int, var s:String)
    val lista: List[MyCaseClass] = List( MyCaseClass(1,"1"),  MyCaseClass(2, "2"))

    assertResult("12"){
      lista.map(x=>x.s).fold(""){(acc,item)=>acc+item}
    }
  }

  test("test - obtenga el promedio de los numeros pares") {
    val lista = List(1, 2, 3, 4, 6, 7, 8, 9, 10)
    assertResult(6) {
      val par = lista.filter(numero =>
        numero % 2 == 0
      )
      println(par)
      val sum = par.fold(0) { (acumulado, item) =>
        acumulado + item
      }
      sum / lista.filter(_%2==0).size
    }

    // otra forma de hacerlo
    assert(6 == lista.filter(_%2==0).fold(0){(a,b)=> a+b} / lista.filter(_%2==0).size)
  }

  test("Una lista se debe poder dividir") {
    val lista = List(1, 2, 3, 4)
    val t: (List[Int], List[Int]) = lista.splitAt(2)
    assert(t._1 == List(1, 2) && t._2 == List(3, 4))
  }

  test("Una lista se debe poder reversar") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(4, 3, 2, 1)) {
      lista.reverse
    }
  }

  test("Una lista se debe poder serializar") {
    val lista = List(1, 2, 3, 4)
    assertResult("1&2&3&4") {
      lista.mkString("&")
    }
  }

  // ----------------------------------

  test("Se debe poder acceder al primer elemento de List() de forma segura") {
    val lista = List()
    val result = lista.headOption
    assert(result == None)
  }

  test("Se debe poder acceder al primer elemento de List() no vacia de forma segura") {
    val lista = List(1,2,3,4)
    val result = lista.headOption
    assert(result == Some(1))
  }

  test("Una List se debe poder transformar") {
    def f(s:String):String = s+"prueba"

    val lista = List("1", "2", "3")
    val lista2 = lista.map(dato => dato + "prueba")
    val lista3 = lista.map(dato => f(dato))

    assert(lista2.head == "1prueba")
    assert(lista != lista2)
    assert(lista2 == lista3)
  }

  test("Verificacion de map sobre una List"){
    case class MyCaseClass(nro:Int)
    val l = List(1, 2, 3)

    val r = l.map(numero => MyCaseClass(numero))

    assert(r == List(MyCaseClass(1),MyCaseClass(2),MyCaseClass(3)))
  }

  test("Una list de string que se transforma en otra list con la longuitud de cada string"){

    def numCaracter(s:String):Int = s.length

    val lista = List("Santiago", "Camilo")
    val lista2 = lista.map(dato => numCaracter(dato))

    assert(lista2 == List(8,6))
  }

  //una lista de lista, crear el promedio de las listas
  test("flatmap de lista, sacar el promedio"){
    val lista: Seq[List[Int]] = List(List(1,2), List(2,2), Nil)

    val lista2 = lista.filter(!_.isEmpty).flatMap(x => List(x.sum/x.size))

    val lista3 = lista.filter(!_.isEmpty).map(x => x.sum/x.size)

    assert(lista2 == List(1,2))
    assert(lista3 == List(1,2))
  }

  test("prueba de scanLeft"){
    val numbers = List(1,2,3,4,5)
    val resul = numbers.scanLeft(0)((a, b) => a + b)
    println(s"ScanLeft: $resul")
    assert(List(0, 1, 3, 6, 10, 15) === resul)
  }
}
