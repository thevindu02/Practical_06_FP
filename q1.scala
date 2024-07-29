object  InventorySystem {

    type Product = (String,Int,Double)
    type Inventory = Map[Int, Product]

    val inventory1: Inventory = Map (
        101 -> ("Product A",10,15.50),
        102 -> ("Product B",5,25.00),
        103 -> ("Product C", 8, 10.00)

    )
    val inventory2: Inventory = Map (
        102 -> ("Product B", 3, 27.00),
        104 -> ("Product D", 7, 22.50),
        105 -> ("Product E", 4, 30.00)
    )

    def getAllProductNames(inventory: Inventory): Iterable[String] = {
        inventory.values.map(_._1)
    }

    def calculateTotalValue(inventory: Inventory): Double = {
        inventory.values.map { case (_, quantity, price) => quantity * price }.sum
    }

    def isInventoryEmpty(inventory: Inventory): Boolean = {
        inventory.isEmpty
    }

    def mergeInventories(inventory1: Inventory, inventory2: Inventory): Inventory = {
        inventory2.foldLeft(inventory1) {
        case (acc, (id, (name, quantity, price))) =>
            acc.get(id) match {
            case Some((_, existingQuantity, existingPrice)) =>
                acc.updated(id, (name, existingQuantity + quantity, math.max(existingPrice, price)))
            case None =>
                acc + (id -> (name, quantity, price))
                }
            }
    }

    def checkProductExists(inventory: Inventory, productId: Int): Unit = {
        inventory.get(productId) match {
            case Some((name, quantity, price)) =>
                println(s"Product ID: $productId")
                println(s"Name: $name")
                println(s"Quantity: $quantity")
                println(s"Price: $$${price}")
            case None =>
            println(s"Product with ID $productId does not exist.")
        }
    }

    def main(args: Array[String]): Unit = {
            val productNames = getAllProductNames(inventory1)
            println("Product names in inventory1: " + productNames.mkString(", "))

            val totalValue = calculateTotalValue(inventory1)
            println(s"Total value of all products in inventory1: $$${totalValue}")

            println("Is inventory1 empty? " + isInventoryEmpty(inventory1))

            val mergedInventory = mergeInventories(inventory1, inventory2)
            println("Merged inventory: " + mergedInventory)

            checkProductExists(inventory1, 102)

    }

}