import io.Source
import scala.annotation.tailrec

object Pizza {
	
	val MIN_ORDERS = 0

	val MAX_ORDERS = 10000

	val MIN_PIZZA_SIZE = 1

	val MAX_PIZZA_SIZE = 1000 * 1000 * 1000

	val MIN_TIME = 0

	val MAX_TIME = 1000 * 1000 * 1000

	val PARSE_ORDER_RE = "\\s{2}".r

	val DEBUG_OPT = "--debug"

	val HELP_OPT = "--help"

	var DEBUG = false

	def isOrdersQueueSizeValid(size: Int) = size >= MIN_ORDERS && size <= MAX_ORDERS

	def isTimeValid(time: Int) = time >= MIN_TIME && time <= MAX_TIME

	def isPizzaSizeValid(size: Int) = size >= MIN_PIZZA_SIZE && size <= MAX_PIZZA_SIZE

	case class Order(time: Int, size: Int)

	def main(args: Array[String]): Unit = {

		if(args.contains(DEBUG_OPT)) {
			DEBUG = true
		}

		if(args.contains(HELP_OPT)) {
			printHelp()
			System.exit(0)
		}

		val result = process(Source.stdin.getLines.toList)

		println(result)

	}

	def printHelp() {
		println(
			"""
				|It calculates mininimal averege waiting time for queue.
				|Input data format:
				|N - total number of orders
				|T1  L1 - the moment of time when customer has entered and time to cook this pizza
				|....
				|Tn  Ln
				|
				|Options:
				|--help - prints this document
				|--debug - more verbose error info
			""".stripMargin
		)
	}

	def process(data: List[String]): Int = {
		val queueSize = data.headOption.getOrElse(throw new Exception("No data")).trim.toInt
		if(! isOrdersQueueSizeValid(queueSize)) {
			throw new Exception("Enter order size $MAX_ORDERS <= N >= $MIN_ORDERS")
		}
		val ordersRaw = data.tail
		if(ordersRaw.size != queueSize) {
			throw new Exception("Orders queue size != $queueSize")
		}

		val orders = ordersRaw.map(parseOrder).sortBy(_.time)

		if(queueSize == 0){
			0
		} else {
			val first = orders.head
			val total = calcTotalWaitingTime(orders.tail, first.size, first.time + first.size)
			(total / queueSize).toInt
		}
	}

	@tailrec
	def calcTotalWaitingTime(rest: List[Order], accWaitingTime: Int, currentTime: Int): Int = {
		if (rest.isEmpty) {
			accWaitingTime
		} else {
			val (next, nextWT) = {
				val waitingOrders = rest.filter(_.time <= currentTime)
				if (waitingOrders.isEmpty) {
					(rest.head, rest.head.size)
				} else {
					val min = waitingOrders.minBy(_.size)
					(min, (currentTime - min.time) + min.size)
				}
			}
			val todoList = rest.filter(_.time != next.time)
			calcTotalWaitingTime(todoList, accWaitingTime + nextWT, currentTime + nextWT)
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
}