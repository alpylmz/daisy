import daisy.lang._
import Real._
import daisy.lang.Vector._

object harmonic {

	def harmonic(x: Vector, y: Vector): Vector = {
require(x >= -5.32 && x <= 725.6 && x.size(100)
	 && x.specV(Set(((0, 3),(215.14, 606.67)), ((6, 8),(26.47, 661.86)), ((10, 12),(224.57, 438.82)),
((13, 23),(57.75, 459.81)), ((24, 24),(497.71, 515.32)), ((26, 36),(462.71, 646.03)),
((38, 38),(448.62, 620.78)), ((42, 48),(138.02, 444.0)), ((51, 61),(340.37, 407.96)),
((72, 82),(25.17, 680.68)), ((4, 4),(60.02, 183.73)), ((5, 5),(18.02, 338.72)),
((9, 9),(311.33, 701.02)), ((25, 25),(181.78, 711.06)), ((37, 37),(335.71, 599.8)),
((39, 39),(50.55, 610.53)), ((40, 40),(426.08, 578.44)), ((41, 41),(380.11, 723.75)),
((49, 49),(30.19, 242.73)), ((50, 50),(3.07, 227.75)), ((62, 62),(108.04, 288.67)),
((63, 64),(377.05, 705.69)), ((65, 66),(289.26, 665.11)), ((67, 68),(9.69, 437.08)),
((69, 69),(52.95, 230.51)), ((70, 71),(246.63, 282.54)), ((83, 84),(109.07, 531.9)),
((87, 87),(96.6, 497.34)), ((89, 90),(73.17, 263.22)), ((97, 97),(201.09, 716.78))))
	 && y >= -432.12 && y <= 78.94 && y.size(100)
	 && y.specV(Set(((0, 0),(-181.96, -115.83)), ((1, 5),(-311.89, -41.89)), ((6, 6),(-197.91, -33.51)),
((7, 8),(-340.35, -190.28)), ((9, 15),(-377.4, -366.47)), ((16, 16),(-385.25, -212.06)),
((42, 44),(-206.25, -171.4)), ((45, 49),(-411.29, -106.86)), ((51, 51),(-390.09, -57.27)),
((52, 62),(-170.72, -34.7)), ((20, 23),(-237.84, -29.81)), ((24, 24),(-360.95, -196.4)),
((26, 29),(-61.42, 36.72)), ((33, 33),(-159.34, 49.51)), ((34, 36),(-69.78, -68.38)),
((38, 41),(-369.92, -98.63)), ((50, 50),(-353.2, 17.15)), ((63, 66),(-316.09, -95.71)),
((67, 70),(-389.68, -216.55)), ((71, 71),(7.03, 20.67)), ((72, 73),(-288.08, -205.94)),
((74, 76),(-363.92, -157.1)), ((77, 79),(-242.62, -34.42)), ((80, 80),(-228.83, -151.35)),
((81, 84),(-326.18, -37.56)), ((85, 88),(-145.13, -16.91)), ((89, 91),(-202.68, -188.52)),
((92, 93),(-390.83, -213.82)), ((94, 96),(-184.74, -62.08)), ((97, 99),(21.25, 73.21))))
	)

        //x1 := x1 + 0.01 * x2
        val x1: Real = y.fold(x.head)((acc: Real, xi: Real) => {acc + 0.01* xi})
        //x2 := -0.01 * x1 + 0.99 * x2
        val x2: Real = x.fold(y.head)((acc: Real, xi: Real) => {-0.01 * xi + 0.99 * acc})
        Vector(List(x1, x2))
    }


}