package example

import example.Promise.Network
import org.scalatest._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.concurrent.duration._

class PromiseSpec extends FlatSpec with Matchers {
  "The future object" should "send a message" in {
    val future = Future {
      Thread.sleep(100)
      println("done")
    }

    future.onComplete {
      case Success(func) => println(s"func: $func")
      case Failure(exception) => println(s"exc: $exception")
    }

    Await.result(future, 2.seconds)
  }

  "The network" should "work" in {
    for {
      john <- Network.fetchProfile(1).fallbackTo(Network.fetchProfile(3))
      elvis <- Network.fetchFriend(john)
    } john.send(elvis)

    Thread.sleep(1000)
  }
}
