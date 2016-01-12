# SJN
This script illustrates 'Shortest Job Next' alg.
The Pizza.scala reads from stgin lines in the next format
N
T1  L1
...
Tn  Ln

Where:
N - total number of orders, max = 10^5
Ti - moment of time when the order has been got, min = 0, max = 10^9,
Li - time which is required to handle the order, min = 1, max = 10^9


To run:
cat Sample00_9.txt | scala Pizza.scala 

Output is a minimum average waiting time.

To run with random test data use:
scala Pizza.scala --test
