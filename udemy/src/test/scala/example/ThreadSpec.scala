package example

import org.scalatest._

class ThreadSpec extends FlatSpec with Matchers {
  "The volatile keyword" should "prevent race condition" in {

    for (_ <- 1 to 10000) {
      val account = new BankAccount(50000)
      val thread1 = new Thread(() => account.buy("shoes", 3000))
      val thread2 = new Thread(() => account.buy("hat", 4000))

      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()
      if (account.amount != 43000)
        throw new RuntimeException("Race condition")
    }
  }
}
