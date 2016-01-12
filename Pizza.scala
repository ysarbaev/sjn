import io.Source
import scala.annotation.tailrec
import scala.collection.SortedMap

object Pizza {
	
	val MIN_ORDERS = 0

	val MAX_ORDERS = 10000

	val MIN_PIZZA_SIZE = 1

	val MAX_PIZZA_SIZE = 1000 * 1000 * 1000

	val MIN_TIME = 0

	val MAX_TIME = 1000 * 1000 * 1000

	val PARSE_ORDER_RE = "\\s+".r

	val TEST_OPTION = "--test"

	def isOrdersQueueSizeValid(size: Int) = size >= MIN_ORDERS && size <= MAX_ORDERS

	def isTimeValid(time: Int) = time >= MIN_TIME && time <= MAX_TIME

	def isPizzaSizeValid(size: Int) = size >= MIN_PIZZA_SIZE && size <= MAX_PIZZA_SIZE

	case class Order(time: BigInt, size: BigInt)

	def main(args: Array[String]): Unit = {

		if (args.contains(TEST_OPTION)) {
			test()
			System.exit(0)
		}
		val result = process(Source.stdin.getLines.toList)
		println(result)
	}

	def process(data: List[String]): Int = {
		val queueSize = data.headOption.getOrElse(throw new Exception("No data")).trim.toInt
		if(! isOrdersQueueSizeValid(queueSize)) {
			throw new Exception(s"Enter order size $MAX_ORDERS <= N >= $MIN_ORDERS")
		}
		val ordersRaw = data.tail
		if(ordersRaw.size != queueSize) {
			throw new Exception(s"Orders queue size expected $queueSize, but ${ordersRaw.size}")
		}

		val orders = ordersRaw.map(parseOrder)
		if(queueSize == 0){
			0
		} else {
			val total = calcTotalWaitingTime(orders)
			(total / queueSize).toInt
		}
	}

	def calcTotalWaitingTime(orders: List[Order]): BigInt = {
		val first = orders.head
		val tree = SortedMap(orders.map(o => o.time -> o): _*) - first.time
		calcTotalWaitingTimeRec(tree, first.size, first.time + first.size)
	}

	//Uses TreeMap to take subtrees more effective, in a typical 
	//situation it will require O(NLogN) operations, however in the worst 
	//case it will be O(N^2)
	@tailrec
	def calcTotalWaitingTimeRec(tree: SortedMap[BigInt, Order], accWaitingTime: BigInt, currentTime: BigInt): BigInt = {
		if (tree.isEmpty) {
			accWaitingTime
		} else {
			val (next, nextWT) = {
				val waitingOrders = tree.to(currentTime)
				if (waitingOrders.isEmpty) {
					val min = tree(tree.keySet.min)
					(min, min.size)
				} else {
					val min = waitingOrders.values.minBy(_.size)
					(min, (currentTime - min.time) + min.size)
				}
			}
			val todo = tree - next.time
			calcTotalWaitingTimeRec(todo, accWaitingTime + nextWT, currentTime + nextWT)
		}
	}

	def orderErr(data: String) = throw new Exception(s"Invalid order: $data")

	def parseOrder(data: String): Order = {
		val arr = PARSE_ORDER_RE.split(data.trim)
		arr match {
			case Array(time, size) => {
				val timeI = time.toInt
				val sizeI = size.toInt

				if (isTimeValid(timeI) && isPizzaSizeValid(sizeI)) {
					Order(timeI, sizeI)
				} else {
					orderErr(data)
				}
			} 
			case _ => orderErr(data)
		}
	}


	def test() {
		import scala.util.Random
		def genOrders(orders: Int, maxTimeDistance: Int, maxSize: Int): List[Order] = {
			var i = 10
			(0 to orders).map { j =>
				val o = Order(i, Random.nextInt(maxSize) + 1)
				i += Random.nextInt(maxTimeDistance) + 1
				o
			}.toList
		}

		def time(vol: Int)(block: Int => Any) {
			val t = System.currentTimeMillis()
			val result = block(vol)
			val tt = (System.currentTimeMillis() - t) / 1000
			println(s"$vol, time = $tt, $result")
		}

		time(1000) { vol =>
			calcTotalWaitingTime(genOrders(vol, 100, 1000)) / vol
		}
		time(10000) { vol =>
			calcTotalWaitingTime(genOrders(vol, 100, 1000)) / vol
		}
		time(20000) { vol =>
			calcTotalWaitingTime(genOrders(vol, 100, 1000)) / vol
		}
	}
}