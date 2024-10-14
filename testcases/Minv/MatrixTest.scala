import daisy.lang._
import Real._
import daisy.lang.Vector._

object MatrixTest {

	def matrixTest(m: Matrix): Real = {
require(m >= -326.68 && m <= 677.57 && m.size(4,2)
	 && m.specM(Set(
        (Set((0, 0)),(-41.42, 7.32)),       (Set((0, 1)),(-307.64, -199.11)),
		(Set((1, 0)),(-145.12, -10.68)),    (Set((1, 1)),(573.71, 625.55)),
		(Set((2, 0)),(283.56, 430.62)),     (Set((2, 1)),(-321.9, -76.17)),
		(Set((3, 0)),(4.99, 96.14)),        (Set((3, 1)),(-199.84, -13.72))))
	)
        val m2 = m + m
        m2.at(1, 1)
    }

}