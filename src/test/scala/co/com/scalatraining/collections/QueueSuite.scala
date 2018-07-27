package co.com.scalatraining.collections
import scala.collection.mutable.Queue

import org.scalatest.FunSuite

class QueueSuite extends FunSuite{

  test("Creacion vacia") {
    val cola = Queue[Int]()
    println(cola)
    assert(cola.isEmpty)
  }

  test("Creacion de cola") {
    val cola = Queue[Int](1,2,3,4)
    println(cola)
    assert(cola === Queue(1,2,3,4))
  }

  test("Agregar elementos a la cola"){
    val cola = Queue(1,2,3)
    assertResult(Queue(1,2,3,4)) {
      cola += 4
    }
  }

  /*test("Agregar elementos a la cola con enqueue"){
    var cola = Queue[String]("1","2","3")
    assertResult(Queue("1","2","3","4")) {
      cola.enqueue("4")
    }
  }*/

  test("Agregar elementos a la cola desde una lista"){
    val cola = Queue("Santiago", "Camilo", "Juan")
    assertResult(Queue("Santiago", "Camilo", "Juan", "Mauricio", "Miguel")) {
      cola ++= List("Mauricio", "Miguel")
    }
  }

  test("Eliminar de la cola el primero"){
    val cola = Queue("Santiago", "Camilo", "Juan")

    cola.dequeue()

    assert(Queue("Camilo", "Juan") === cola)
  }

  test("Eliminar de la cola el primero que cumpla con la condición"){
    val cola = Queue("Santiago", "Camilo", "Juan")

    cola.dequeueFirst(_.startsWith("C"))

    assert(Queue("Santiago", "Juan") === cola)
  }

  test("Eliminar de la cola todos los que cumplan con la condición"){
    val cola = Queue("Santiago", "Camilo", "Juan", "Cristina")

    cola.dequeueAll(_.startsWith("C"))

    assert(Queue("Santiago", "Juan") === cola)
  }

  test("Recorrer una cola con foreach"){
    val cola = Queue(1, 2, 3, 4)
    var sum = 0
    assertResult(10){
      cola.foreach((x) =>
        sum += x
      )
      sum
    }
  }

  test("Head en un Queue"){
    val cola = Queue(1,2,3)
    assertResult(1){
      cola.head
    }
  }

  test("tail en un Queue"){
    val cola = Queue(1,2,3)
    assertResult(Queue(2,3)){
      cola.tail
    }
  }

  test("split en un Queue"){
    val cola = Queue(1,2,3)
    val (cola2, cola3) = cola.splitAt(2)
    assert(cola2 === Queue(1,2) && cola3 === Queue(3))
  }

  test("filter en un Queue"){
    val cola = Queue(1,2,3,4,5,6,7,8,9,10)
    assertResult(Queue(2,4,6,8,10)){
      cola.filter(dato =>
        dato % 2 == 0
      )
    }
  }

  test("Transformación de un Queue con map"){
    val cola = Queue(1,2,3,4,5)
    assertResult(Queue(2,4,6,8,10)){
      cola.map(dato =>
        dato * 2
      )
    }
  }

  test("Obtener los ultimos de un Queue"){
    val cola = Queue(1,2,3,4,5,6,7,8)
    assertResult(Queue(7,8)){
      cola.takeRight(2)
    }
  }

  test("Obtener los primeros de un Queue"){
    val cola = Queue(1,2,3,4,5,6,7,8)
    assertResult(Queue(1,2)){
      cola.take(2)
    }
  }
}
