import daisy.lang._
import Real._
import daisy.lang.Vector._

object nonlin2 {

	def nonlin2(x: Vector, y: Vector): Vector = {
require(x >= 0.0 && x <= 1.0 && x.size(100)
	 && x.specV(Set(((0, 0),(0.01, 0.55)), ((1, 11),(0.14, 0.94)), ((19, 29),(0.2, 0.61)),
((30, 30),(0.51, 0.64)), ((38, 42),(0.17, 0.81)), ((43, 43),(0.3, 0.31)),
((46, 46),(0.05, 0.89)), ((47, 49),(0.72, 0.78)), ((52, 62),(0.01, 0.84)),
((63, 73),(0.47, 0.75)), ((12, 14),(0.73, 0.78)), ((15, 15),(0.56, 0.72)),
((16, 17),(0.85, 0.95)), ((18, 18),(0.28, 0.89)), ((31, 31),(0.38, 0.42)),
((32, 33),(0.25, 0.62)), ((34, 36),(0.1, 0.9)), ((37, 37),(0.1, 0.35)),
((44, 45),(0.4, 0.88)), ((50, 51),(0.24, 0.34)), ((74, 74),(0.02, 0.21)),
((75, 77),(0.28, 0.28)), ((78, 78),(0.29, 0.48)), ((79, 80),(0.25, 0.76)),
((84, 84),(0.38, 0.82)), ((85, 87),(0.31, 0.45)), ((90, 92),(0.23, 0.93)),
((94, 94),(0.83, 0.94)), ((95, 95),(0.08, 0.97)), ((96, 98),(0.17, 0.87))))
	 && y >= 0.0 && y <= 1.0 && y.size(100)
	 && y.specV(Set(((0, 2),(0.24, 0.33)), ((3, 3),(0.63, 0.91)), ((4, 4),(0.67, 0.69)),
((5, 5),(0.47, 0.98)), ((7, 8),(0.7, 0.84)), ((9, 10),(0.44, 0.86)),
((12, 19),(0.57, 0.72)), ((20, 21),(0.21, 0.31)), ((22, 28),(0.08, 0.62)),
((50, 60),(0.14, 0.91)), ((6, 6),(0.11, 0.45)), ((11, 11),(0.06, 0.54)),
((29, 32),(0.1, 0.82)), ((33, 33),(0.02, 0.81)), ((34, 36),(0.3, 0.95)),
((37, 40),(0.14, 0.33)), ((41, 43),(0.24, 0.67)), ((44, 47),(0.48, 0.81)),
((48, 48),(0.27, 0.59)), ((49, 49),(0.22, 0.26)), ((61, 63),(0.08, 0.24)),
((64, 65),(0.16, 0.22)), ((67, 68),(0.42, 0.45)), ((69, 72),(0.55, 0.59)),
((82, 84),(0.61, 0.68)), ((85, 85),(0.47, 0.85)), ((86, 86),(0.14, 0.27)),
((88, 89),(0.01, 0.91)), ((90, 90),(0.41, 0.88)), ((93, 94),(0.56, 0.84))))
	)

        //x := x + 0.01 * (-x + 2*x*x + y*y)
        val x1: Real = y.fold(x.head)((acc: Real, yi: Real) => {acc + 0.01 * (-acc + 2*acc*acc + yi*yi)})
        //y := y + 0.01 * (-y + y*y)
        val y1: Real = x.fold(y.head)((acc: Real, xi: Real) => {acc + 0.01 * (-acc + acc*acc)})
        Vector(List(x1, y1))
    }


}