object InventorySystem {
  case class Product(name: String, quantity: Int, price: Double)

  val inventory1: Map[Int, Product] = Map(
    101 -> Product("ProductA", 10, 5.0),
    102 -> Product("ProductB", 20, 3.0),
    103 -> Product("ProductC", 15, 2.0)
  )

  val inventory2: Map[Int, Product] = Map(
    102 -> Product("ProductB", 10, 4.0),
    104 -> Product("ProductD", 5, 10.0)
  )

  def getAllProductNames(inventory: Map[Int, Product]): Iterable[String] = {
    inventory.values.map(_.name)
  }

  def calculateTotalValue(inventory: Map[Int, Product]): Double = {
    inventory.values.map(product => product.quantity * product.price).sum
  }

  def isInventoryEmpty(inventory: Map[Int, Product]): Boolean = {
    inventory.isEmpty
  }

  def mergeInventories(
      inv1: Map[Int, Product],
      inv2: Map[Int, Product]
  ): Map[Int, Product] = {

    val combined = inv2.map { case (id, newProduct) =>
      inv1.get(id) match {
        case Some(existingProduct) =>
          id -> Product(
            name = existingProduct.name,
            quantity = existingProduct.quantity + newProduct.quantity,
            price = math.max(existingProduct.price, newProduct.price)
          )
        case None =>
          id -> newProduct
      }
    }
    inv1 ++ combined
  }

  def checkProductExists(inventory: Map[Int, Product], id: Int): Unit = {
    inventory.get(id) match {
      case Some(product) => println(s"Product ID $id: $product")
      case None          => println(s"Product ID $id does not exist.")
    }
  }

  def main(args: Array[String]): Unit = {
    println("Product Names in Inventory1:")
    println(getAllProductNames(inventory1).mkString(", "))

    println("\nTotal Value of Inventory1:")
    println(calculateTotalValue(inventory1))

    println("\nIs Inventory1 empty?")
    println(isInventoryEmpty(inventory1))

    println("\nMerged Inventory:")
    val mergedInventory = mergeInventories(inventory1, inventory2)
    mergedInventory.foreach { case (id, product) =>
      println(s"Product ID $id: $product")
    }

    println("\nCheck if Product ID 102 exists in Inventory1:")
    checkProductExists(inventory1, 102)
  }
}
