package example

class BankAccount(@volatile var amount: Int) {
	override def toString: String = "" + amount

	def buy(thing: String, price: Int): Unit = {
		amount -= price
	}
}
