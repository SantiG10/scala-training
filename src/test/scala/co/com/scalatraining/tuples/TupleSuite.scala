package co.com.scalatraining.tuples

import org.scalatest.FunSuite

class TupleSuite  extends FunSuite {

  test("Una tupla se debe poder crear"){
    val tupla: (Int, Int, String, List[Int]) = (1, 2,"3", List(1, 2, 3))
    assert(tupla._2 == 2)
    assert(tupla._4.tail.head == 2)
  }

  // crear tupla 5 listas y va tomar de cada lista el head y con cada head crear una tupla

  test("tuplas"){
    val tupla: (List[Int], List[Int], List[Int], List[Int], List[Int]) = (List(1,2,3), List(4,5,6), List(7,8,9), List(10,11,12), List(13,14,15))

    tupla.productIterator.foreach(x => println(x))

    val tupla2 = (tupla._1.tail.head, tupla._2.tail.head, tupla._3.tail.head, tupla._4.tail.head, tupla._5.tail.head)

    assert(tupla2 == (2,5,8,11,14))
  }

}