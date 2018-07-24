package co.com.scalatraining.collections

import org.scalatest.FunSuite

import scala.collection.mutable.Stack

class StackSuite extends FunSuite {

  test("Creacion vacia") {
    val pila = Stack[Int]()
    println(pila)
    assert(pila.isEmpty)
  }

  test("Creacion de pila") {
    val pila = Stack[Int](1,2,3,4)
    println(pila)
    assert(pila === Stack(1,2,3,4))
  }

  test("Agregar elementos a la pila"){
    val pila = Stack(1,2,3)
    assertResult(Stack(4,1,2,3)) {
      pila.push(4)
    }
  }

  test("Eliminar elemento de la pila"){
    val pila = Stack(1,2,3)
    pila.pop()
    assert(pila === Stack(2,3))
  }

  test("Conocer el primero en la pila"){
    val pila = Stack(1,2,3)
    assertResult(1){
      pila.top
    }
  }

  test("Conocer el tamaño de la pila"){
    val pila = Stack(1,2,3)
    assertResult(3){
      pila.size
    }
  }

  test("Limpiar la pila"){
    val pila = Stack(1,2,3)
    pila.clear()
    assert(pila.isEmpty)
  }

  test("tail en un Stack"){
    val cola = Stack(1,2,3)
    assertResult(Stack(2,3)){
      cola.tail
    }
  }

  test("Recorrer una pila con foreach"){
    val pila = Stack(1, 2, 3, 4, 5)
    var sum = 0
    assertResult(15){
      pila.foreach((x) =>
        sum += x
      )
      sum
    }
  }

  test("split en una pila"){
    val pila = Stack(1,2,3)
    val (pila2, pila3) = pila.splitAt(2)
    assert(pila2 === Stack(1,2) && pila3 === Stack(3))
  }

  test("filter en una pila"){
    val pila = Stack(1,2,3,4,5,6,7,8,9,10)
    assertResult(Stack(2,4,6,8,10)){
      pila.filter(dato =>
        dato % 2 == 0
      )
    }
  }

  test("Transformación de una pila con map"){
    val pila = Stack(1,2,3,4,5)
    assertResult(Stack(2,4,6,8,10)){
      pila.map(dato =>
        dato * 2
      )
    }
  }
}
