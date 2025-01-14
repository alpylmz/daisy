import daisy.lang._
import Real._
import daisy.lang.Vector._

object roux1 {

	def roux1(x: Vector): Real = {
require(x >= -58.25 && x <= 61.32 && x.size(100)
	 && x.specV(Set(((16, 25),(-46.01, 60.24)), ((37, 47),(-1.48, 39.69)), ((51, 55),(42.26, 51.08)),
((56, 59),(-53.42, -23.13)), ((60, 60),(-44.0, -35.58)), ((61, 62),(11.48, 58.09)),
((65, 75),(-13.74, 52.97)), ((80, 80),(-4.55, 45.31)), ((81, 83),(-27.17, 34.75)),
((87, 97),(-45.6, 54.83)), ((1, 3),(-6.52, 43.71)), ((5, 7),(-50.97, -28.4)),
((9, 10),(-31.13, -16.32)), ((12, 12),(-20.2, 21.97)), ((26, 26),(-52.91, 27.74)),
((27, 28),(-40.84, 40.06)), ((29, 31),(36.02, 46.44)), ((32, 32),(-37.06, 24.6)),
((35, 35),(-49.25, 22.49)), ((48, 48),(-31.78, 42.16)), ((49, 50),(-2.38, 42.59)),
((64, 64),(28.51, 54.7)), ((76, 76),(19.15, 48.54)), ((77, 77),(-15.58, 15.39)),
((78, 78),(-41.42, 23.47)), ((79, 79),(-48.07, -3.49)), ((84, 85),(-47.25, 21.59)),
((86, 86),(-33.44, -7.29)), ((98, 98),(-23.51, 55.21)), ((99, 99),(0.37, 6.25))))
	)

        x.fold(0.0)((y: Real, i: Real) => {1.5 * i - 0.7 * y})
    }


}