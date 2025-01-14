import daisy.lang._
import Real._
import daisy.lang.Vector._

object stdDeviation {

	def stdDeviation(x: Vector): Real = {
require(x >= -160.06 && x <= 360.98 && x.size(100)
	 && x.specV(Set(((0, 0),(-126.33, -31.2)), ((1, 2),(-142.42, 127.86)), ((3, 13),(9.67, 350.69)),
((30, 40),(300.27, 354.64)), ((41, 51),(-116.25, -22.04)), ((54, 58),(-109.82, 99.67)),
((61, 71),(72.83, 209.76)), ((74, 75),(-38.03, 57.13)), ((76, 78),(163.15, 191.18)),
((79, 82),(229.67, 281.25)), ((14, 15),(40.71, 341.34)), ((16, 17),(295.44, 332.83)),
((18, 18),(29.09, 222.68)), ((19, 20),(167.89, 239.46)), ((26, 26),(16.87, 87.59)),
((27, 28),(-113.65, -78.66)), ((52, 52),(0.75, 254.44)), ((53, 53),(63.65, 205.91)),
((59, 59),(-11.33, 210.36)), ((60, 60),(-114.81, 187.72)), ((72, 73),(-53.5, -26.18)),
((83, 84),(81.36, 98.45)), ((85, 86),(203.09, 303.95)), ((87, 88),(66.87, 69.64)),
((89, 90),(-92.5, 152.47)), ((91, 92),(-4.69, 154.55)), ((93, 93),(153.09, 329.45)),
((94, 95),(-50.75, 344.04)), ((96, 97),(-44.0, 254.76)), ((98, 99),(-142.58, 31.84))))
	)

        val n: Real = x.length()
        val y = x.fold(0.0)((acc: Real, i: Real) => acc + i)
        val avg = y / n

        val z = x.fold(0.0)((acc: Real, i: Real) => {
            acc + pow((i - avg), 2)
        })
        sqrt(z / n)
    }


}