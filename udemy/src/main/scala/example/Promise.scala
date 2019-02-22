package example

import scala.util.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Promise extends App {

	case class Profile(id: Int, name: String) {
		def send(other: Profile): Unit =
			println(s"$name sends message to ${other.name}")
	}

	object Network {
		val nodes = Map (
			1 -> "John",
			2 -> "Elvis",
			3 -> "Nobody"
		)

		val friends = Map (
			1 -> 2,
			2 -> 3
		)

		val random = new Random()

		def fetchProfile(id: Int): Future[Profile] = Future {
			println(s"fetching profile: $id")
			Thread.sleep(random.nextInt(300))
			Profile(id, nodes(id))
		}
		def fetchFriend(profile: Profile): Future[Profile] = Future {
			println(s"fetching friend of: ${profile.name}")
			Thread.sleep(random.nextInt(300))
			val friendId = friends(profile.id)
			Profile(friendId, nodes(friendId))
		}
	}
}
