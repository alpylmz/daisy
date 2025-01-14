import daisy.lang._
import Real._
import daisy.lang.Vector._

object nonlin3 {

	def nonlin3(x: Vector, y: Vector): Vector = {
require(x >= 0.0 && x <= 1.0 && x.size(100)
	 && x.specV(Set(((0, 0),(0.03, 0.11)), ((2, 5),(0.81, 0.85)), ((6, 6),(0.63, 0.83)),
((9, 11),(0.29, 0.63)), ((12, 12),(0.1, 0.1)), ((13, 14),(0.2, 0.76)),
((32, 42),(0.32, 0.42)), ((47, 57),(0.02, 0.26)), ((64, 74),(0.28, 0.72)),
((95, 95),(0.32, 0.37))))
	 && y >= 0.0 && y <= 1.0 && y.size(100)
	 && y.specV(Set(((0, 0),(0.7, 0.83)), ((1, 11),(0.59, 0.97)), ((12, 12),(0.52, 0.96)),
((13, 23),(0.16, 0.58)), ((24, 25),(0.82, 0.94)), ((27, 33),(0.17, 0.23)),
((34, 35),(0.77, 0.94)), ((36, 46),(0.49, 0.99)), ((83, 93),(0.1, 0.56)),
((94, 97),(0.1, 0.33))))
	)

        //x := x + 0.01 * (-x + y*y)
        val x1: Real = y.fold(x.head)((acc: Real, yi: Real) => {acc + 0.01 * (-acc + yi*yi)})
        //y := y + 0.01 * (-2.0*y + 3.0*x*x)
        val y1: Real = x.fold(y.head)((acc: Real, xi: Real) => {acc + 0.01 * (-2.0*acc + 3.0*xi*xi)})
        Vector(List(x1, y1))
    }


}