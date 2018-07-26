package co.com.scalatraining.effects

import java.util.Random
import java.util.concurrent.Executors

import org.scalatest.FunSuite

import scala.collection.immutable.{IndexedSeq, Seq}
import scala.language.postfixOps
import scala.util.{Failure, Success}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class FutureSuite extends FunSuite {

  test("Un futuro se puede crear") {

    val hiloPpal = Thread.currentThread().getName

    var hiloFuture = ""

    println(s"Test 1 - El hilo ppal es ${hiloPpal}")

    val saludo: Future[String] = Future {
      hiloFuture = Thread.currentThread().getName
      println(s"Test 1 - El hilo del future es ${hiloFuture}")

      Thread.sleep(500)
      "Hola"
    }
    val resultado: String = Await.result(saludo, 10 seconds)
    assert(resultado == "Hola")
    assert(hiloPpal != hiloFuture)
  }

  test("map en Future") {


    val t1 = Thread.currentThread().getName
    println(s"Test 2 - El hilo del ppal es ${t1}")


    val saludo = Future {
      val t2 = Thread.currentThread().getName
      println(s"Test 2 - El hilo del future es ${t2}")

      Thread.sleep(500)
      "Hola"
    }

    Thread.sleep(5000)

    val saludo2 = Future {
      println(s"Test 2 - Hilo normal ${Thread.currentThread().getName}")
    }

    val saludoCompleto = saludo.map(mensaje => {
      val t3 = Thread.currentThread().getName
      println(s"Test 2 - El hilo del map es ${t3}")

      mensaje + " muchachos"
    })


    val resultado = Await.result(saludoCompleto, 10 seconds)
    assert(resultado == "Hola muchachos")
  }

  test("Se debe poder encadenar Future con for-comp") {
    val f1 = Future {
      Thread.sleep(200)
      1
    }

    val f2 = Future {
      Thread.sleep(200)
      2
    }

    val f3: Future[Int] = for {
      res1 <- f1
      res2 <- f2
    } yield res1 + res2

    val res = Await.result(f3, 10 seconds)

    assert(res == 3)
  }

  test("Se debe poder encadenar Future con for-comp 2") {
    val f1 = Future {
      Thread.sleep(200)
      1
    }

    val f2 = Future {
      Thread.sleep(200)
      2
    }

    val f3 = Future {
      Thread.sleep(200)
      2 / 0
    }

    val f4: Future[Int] = for {
      res1 <- f1
      res2 <- f2
      res3 <- f3.recover { case e: Exception => 3 }
    } yield res1 + res2 + res3

    val res = Await.result(f4, 10 seconds)

    println(s"Test Falure: $res")
    assert(res == 6)
  }

  test("Se debe poder manejar el error de un Future de forma imperativa") {
    val divisionCero = Future {
      Thread.sleep(100)
      10 / 0
    }
    var error = false

    val r: Unit = divisionCero.onFailure {
      case e: Exception => error = true
    }

    Thread.sleep(1000)

    assert(error == true)
  }

  test("Se debe poder manejar el exito de un Future de forma imperativa") {

    val division = Future {
      5
    }

    var r = 0

    val f: Unit = division.onComplete {
      case Success(res) => r = res
      case Failure(e) => r = 1
    }

    Thread.sleep(150)

    val res = Await.result(division, 10 seconds)

    assert(r == 5)
  }

  test("Se debe poder manejar el error de un Future de forma funcional sincronicamente") {

    var threadName1 = ""
    var threadName2 = ""

    val divisionPorCero = Future {
      threadName1 = Thread.currentThread().getName
      Thread.sleep(100)
      10 / 0
    }.recover {
      case e: ArithmeticException => {
        threadName2 = Thread.currentThread().getName
        "No es posible dividir por cero"
      }
    }

    val res = Await.result(divisionPorCero, 10 seconds)

    assert(threadName1 == threadName2)
    assert(res == "No es posible dividir por cero")

  }

  test("Se debe poder manejar el error de un Future de forma funcional asincronamente") {

    var threadName1 = ""
    var threadName2 = ""

    implicit val ecParaPrimerHilo = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

    val f1 = Future {
      threadName1 = Thread.currentThread().getName
      2 / 0
    }(ecParaPrimerHilo)
      .recoverWith {
        case e: ArithmeticException => {

          implicit val ecParaRecuperacion = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

          Future {
            threadName2 = Thread.currentThread().getName
            1
          }(ecParaRecuperacion)
        }
      }

    val res = Await.result(f1, 10 seconds)

    println(s"Test en recoverWith thread del fallo: $threadName1")
    println(s"Test en recoverWith thread de recuperacion: $threadName2")

    assert(threadName1 != threadName2)
    assert(res == 1)
  }

  test("Los future **iniciados** fuera de un for-comp deben iniciar al mismo tiempo") {

    val timeForf1 = 100
    val timeForf2 = 200
    val timeForf3 = 100

    val additionalTime = 50D

    val estimatedElapsed = (Math.max(Math.max(timeForf1, timeForf2), timeForf3) + additionalTime) / 1000

    val f1 = Future {
      Thread.sleep(timeForf1)
      1
    }
    val f2 = Future {
      Thread.sleep(timeForf2)
      2
    }
    val f3 = Future {
      Thread.sleep(timeForf3)
      3
    }

    val t1: Long = System.nanoTime()

    val resultado = for {
      a <- f1
      b <- f2
      c <- f3
    } yield (a + b + c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    println(s"Future **iniciados** fuera del for-comp estimado: $estimatedElapsed real: $elapsed")
    assert(elapsed <= estimatedElapsed)
    assert(res == 6)

  }

  test("Los future **definidos** fuera de un for-comp deben iniciar secuencialmente") {

    val timeForf1 = 100
    val timeForf2 = 300
    val timeForf3 = 500

    val estimatedElapsed: Double = (timeForf1 + timeForf2 + timeForf3) / 1000

    def f1 = Future {
      Thread.sleep(timeForf1)
      1
    }

    def f2 = Future {
      Thread.sleep(timeForf2)
      2
    }

    def f3 = Future {
      Thread.sleep(timeForf3)
      3
    }

    val t1 = System.nanoTime()

    val resultado = for {
      a <- f1
      b <- f2
      c <- f3
    } yield (a + b + c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    println(s"Future **definidos** fuera del for-comp estimado: $estimatedElapsed real: $elapsed")

    assert(elapsed >= estimatedElapsed)
    assert(res == 6)

  }

  test("Los future declarados dentro de un for-comp deben iniciar secuencialmente") {

    val t1 = System.nanoTime()

    val timeForf1 = 100
    val timeForf2 = 100
    val timeForf3 = 100

    val estimatedElapsed = (timeForf1 + timeForf2 + timeForf3) / 1000

    val resultado = for {
      a <- Future {
        Thread.sleep(timeForf1)
        1
      }
      b <- Future {
        Thread.sleep(timeForf2)
        2
      }
      c <- Future {
        Thread.sleep(timeForf3)
        3
      }
    } yield (a + b + c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    assert(elapsed >= estimatedElapsed)
    assert(res == 6)
  }

  test("Future.sequence") {

    val listOfFutures: List[Future[Int]] = Range(1, 11).map(Future.successful(_)).toList

    val resSequence: Future[List[Int]] = Future.sequence {
      listOfFutures
    }

    val resFuture = resSequence.map(l => l.sum / l.size)

    val res = Await.result(resFuture, 10 seconds)

    assert(res == Range(1, 11).sum / Range(1, 11).size)

  }

  test("Future.traverse") {
    def foo(i: List[Int]): Future[Int] = Future.successful(i.sum / i.size)

    val resFuture = Future.traverse(Range(1, 11).map(Future.successful(_))) {
      x => x
    }.map(l => l.sum / l.size)

    val res = Await.result(resFuture, 10 seconds)

    assert(res == Range(1, 11).sum / Range(1, 11).size)

  }

  /* servcio del clima, que podemos consultar
     1. Servicio del clima (5) -> maximo 5 veces
     2. Guardar BD del valor (1) -> maximo 1 vez
   */

  test("Consultar el clima") {

    def clima(): String = "13°C"

    def guardar(i: String): String = s"Guarde en la BD: $i"

    var threadName1 = ""
    var threadName2 = ""
    var c = ""
    var g = ""

    val ecHiloClima = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))
    val ecHiloGuardar = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

    /*(1 to 10).foreach{ _ =>
      val f1 = Future {
        threadName1 = Thread.currentThread().getName
        c = clima()
        //println(c)
        println(s"Hilo clima: $threadName1")
        println("Tiempo ejecución: " + System.nanoTime() / 1.0E09)
        Thread.sleep(1000)
      }(ecHiloClima)

      val f2 = Future {
        threadName2 = Thread.currentThread().getName
        g = guardar(c)
        //println(g)
        println(s"Hilo guardar: $threadName2")
        println("Tiempo ejecución: " + System.nanoTime() / 1.0E09)
        Thread.sleep(1000)
      }(ecHiloGuardar)
    }*/

    def getClima(): Future[Int] = Future {
      threadName1 = Thread.currentThread().getName
      c = clima()
      //println(c)
      println(s"Hilo clima: $threadName1")
      println("Tiempo ejecución: " + System.nanoTime() / 1.0E09)
      Thread.sleep(500)
      13
    }(ecHiloClima)

    def saveBD(x: Int): Future[String] = Future {
      threadName2 = Thread.currentThread().getName
      g = guardar(c)
      //println(g)
      println(s"Hilo guardar: $threadName2")
      println("Tiempo ejecución: " + System.nanoTime() / 1.0E09)
      Thread.sleep(500)
      s"Guarde en la BD: $x"
    }(ecHiloGuardar)

    val a = Future.sequence {
      Range(1, 11).map(x => getClima().flatMap(srt => saveBD(srt)))
    }

    //Range(1,11).map(x => saveBD(x))
  }

  /* 1. Consulta de repositorio x usuario
     2. Consulta de detalles de repositorio - lenguaje - lineas de codigo
     3. Ordenar por tamaño y despues mostrar por lenguaje y contar los resultados
     Salida
     a - b -c
     Scala -> 2
     Ruby  -> 1
   */

  test("test prueba de repositorios con git") {

    case class UsuarioCompleto(nombre: String, repositorios: List[String])
    case class Repositorios(nombre: String, lenguaje: String, lineas: Int)

    val user1 = UsuarioCompleto("Santiago Giraldo", List("scala-1", "ruby-1", "scala-2"))
    val user2 = UsuarioCompleto("Mauricio Serna", List("scala-3", "ruby-2", "ruby-3"))

    val repo1 = Repositorios("scala-1", "scala", 1000)
    val repo2 = Repositorios("scala-2", "scala", 1500)
    val repo3 = Repositorios("scala-3", "scala", 500)
    val repo4 = Repositorios("ruby-1", "ruby", 1010)
    val repo5 = Repositorios("ruby-2", "ruby", 900)
    val repo6 = Repositorios("ruby-3", "ruby", 100)

    val listUsers = List(user1, user2)
    val listRepos = List(repo1, repo2, repo3, repo4, repo5, repo6)

    object UserService {
      implicit val ex = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

      def UserRepositories(nombre: String): Future[List[UsuarioCompleto]] = {
        Future {
          val threadName = Thread.currentThread().getName
          //println(s"Nombre Hilo: $threadName")
          //println("Tiempo ejecución: " + System.nanoTime() / 1.0E09)
          //Thread.sleep(500)
          val userCompleto = listUsers.filter(x => x.nombre == nombre)
          userCompleto
        }
      }
    }

    object DetalleReposService {
      implicit val ex = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))

      def DetailRepo(nombre: String): Future[List[Repositorios]] = {
        Future {
          val threadName = Thread.currentThread().getName
          //println(s"Nombre Hilo: $threadName")
          //println("Tiempo ejecución: " + System.nanoTime() / 1.0E09)
          //Thread.sleep(500)
          val detail = listRepos.filter(x => x.nombre == nombre)
          detail
        }
      }
    }

    val users = List("Santiago Giraldo", "Mauricio Serna")

    users.foreach { x =>
      println(s"\nUser: ${x}")
      val a = UserService.UserRepositories(x)
      val res = Await.result(a, 10 seconds)

      println("aca:" + res)

      val repos = Future.sequence {
        res.flatMap(x => x.repositorios).map { y => DetalleReposService.DetailRepo(y) }
      }

      val res1 = Await.result(repos, 10 seconds)

      val detailRepos = res1.flatten.groupBy(x => x.lineas).toSeq.sortWith(_._1 > _._1)

      println("--------Resultado GIT---------")

      val orderMayorLineas = detailRepos.map(x => x._1 -> x._2.map(y => y.nombre))

      println("Orden de repositorio por contidad de lineas")
      orderMayorLineas.foreach(x =>
        println(s"${x._2.head} -> ${x._1}")
      )

      println("\nCanridad de lenguajes")

      val lenguajes = res1.flatten.groupBy(x => x.lenguaje).mapValues(_.size).toSeq.sortWith(_._2 > _._2)

      lenguajes.foreach(x =>
        println(s"${x._1} -> ${x._2}")
      )
      println("-------------------------------")
    }

    //println(s"Git: " + lenguajes)
  }
}