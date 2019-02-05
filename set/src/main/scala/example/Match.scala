package example

class Person(val name: String, val age: Int)

object Pattern {
	def unapply(person: Person): Option[(String, Int)] =
		if(person.age < 18) None
		else Some((person.name, person.age))

	def unapply(age: Int): Option[String] =
		Some(if(age == 43) "Kris" else "Stranger")
}

object Match extends App {
	val kris = new Person("Kris", age = 43)
	val greeting = kris match {
		case Pattern(n, a) => s"Hello from $n, aged $a"
	}
	println(greeting)
	println(kris.age match {
		case Pattern(age) => age
	})
}
