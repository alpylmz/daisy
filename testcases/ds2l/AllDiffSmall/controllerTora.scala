import daisy.lang._
import Real._
import daisy.lang.Vector._

object controllerTora {

	def controllerTora(x: Vector, weights1: Matrix, weights2: Matrix, weights3: Matrix, weights4: Matrix, bias1: Vector, bias2: Vector, bias3: Vector, bias4: Real): Vector = {
require(10.197819 <= bias4 && bias4 <= 10.197819
	 && x >= -2.0 && x <= 2.0 && x.size(10)
	 && x.specV(Set(((0, 0),(1.38, 1.96)), ((1, 1),(0.14, 0.98)), ((2, 2),(-0.77, 1.89)),
((3, 3),(0.09, 0.61)), ((4, 4),(-0.87, 1.06)), ((5, 5),(-1.66, -0.97)),
((6, 6),(-1.47, 1.38)), ((7, 7),(-1.06, -0.27)), ((8, 8),(-0.75, 1.82)),
((9, 9),(0.77, 1.99))))
	 && bias1 >= 0.040232 && bias1 <= 0.341392 && bias1.size(10)
	 && bias1.specV(Set(((0, 0),(0.08, 0.2)), ((1, 1),(0.18, 0.2)), ((2, 2),(0.05, 0.12)),
((3, 3),(0.15, 0.32)), ((4, 4),(0.09, 0.18)), ((5, 5),(0.14, 0.16)),
((6, 6),(0.18, 0.29)), ((7, 7),(0.18, 0.24)), ((8, 8),(0.19, 0.21)),
((9, 9),(0.09, 0.27))))
	 && bias2 >= 0.082624 && bias2 <= 0.318763 && bias2.size(10)
	 && bias2.specV(Set(((0, 0),(0.09, 0.17)), ((1, 1),(0.18, 0.19)), ((2, 2),(0.12, 0.25)),
((3, 3),(0.14, 0.17)), ((4, 4),(0.09, 0.12)), ((5, 5),(0.1, 0.26)),
((6, 6),(0.21, 0.23)), ((7, 7),(0.21, 0.31)), ((8, 8),(0.29, 0.318763)),
((9, 9),(0.21, 0.318763))))
	 && bias3 >= 0.096189 && bias3 <= 0.297542 && bias3.size(10)
	 && bias3.specV(Set(((0, 0),(0.11, 0.23)), ((1, 1),(0.21, 0.28)), ((2, 2),(0.19, 0.22)),
((3, 3),(0.1, 0.16)), ((4, 4),(0.15, 0.2)), ((5, 5),(0.17, 0.29)),
((6, 6),(0.24, 0.26)), ((7, 7),(0.15, 0.2)), ((8, 8),(0.14, 0.25)),
((9, 9),(0.21, 0.23))))
	 && weights1 >= -0.374036 && weights1 <= 0.319683 && weights1.size(10,10)
	 && weights1.specM(Set((Set((0, 0)),(-0.36, -0.33)), (Set((0, 1)),(-0.07, 0.27)),
		(Set((0, 2)),(-0.06, 0.02)), (Set((0, 3)),(-0.0, 0.03)),
		(Set((0, 4)),(-0.02, 0.2)), (Set((0, 5)),(0.2, 0.3)),
		(Set((0, 6)),(-0.0, 0.09)), (Set((0, 7)),(-0.28, -0.15)),
		(Set((0, 8)),(-0.03, 0.07)), (Set((0, 9)),(-0.2, 0.1)),
		(Set((1, 0)),(-0.29, -0.27)), (Set((1, 1)),(-0.37, -0.18)),
		(Set((1, 2)),(-0.34, 0.2)), (Set((1, 3)),(-0.1, 0.14)),
		(Set((1, 4)),(-0.19, -0.11)), (Set((1, 5)),(0.14, 0.14)),
		(Set((1, 6)),(-0.24, -0.07)), (Set((1, 7)),(-0.31, 0.0)),
		(Set((1, 8)),(0.01, 0.27)), (Set((1, 9)),(-0.04, 0.319683)),
		(Set((2, 0)),(-0.04, 0.15)), (Set((2, 1)),(0.05, 0.07)),
		(Set((2, 2)),(-0.13, 0.12)), (Set((2, 3)),(0.09, 0.18)),
		(Set((2, 4)),(0.04, 0.16)), (Set((2, 5)),(-0.13, -0.1)),
		(Set((2, 6)),(-0.29, 0.18)), (Set((2, 7)),(-0.19, 0.31)),
		(Set((2, 8)),(-0.15, 0.03)), (Set((2, 9)),(-0.37, -0.12)),
		(Set((3, 0)),(-0.22, -0.04)), (Set((3, 1)),(-0.29, -0.06)),
		(Set((3, 2)),(-0.28, 0.24)), (Set((3, 3)),(-0.26, 0.06)),
		(Set((3, 4)),(-0.08, 0.22)), (Set((3, 5)),(-0.3, -0.15)),
		(Set((3, 6)),(-0.27, 0.09)), (Set((3, 7)),(-0.16, 0.13)),
		(Set((3, 8)),(-0.26, 0.07)), (Set((3, 9)),(-0.28, -0.23)),
		(Set((4, 0)),(-0.16, 0.29)), (Set((4, 1)),(-0.08, 0.26)),
		(Set((4, 2)),(-0.13, 0.05)), (Set((4, 3)),(-0.31, 0.13)),
		(Set((4, 4)),(-0.14, 0.03)), (Set((4, 5)),(-0.21, 0.06)),
		(Set((4, 6)),(-0.18, 0.13)), (Set((4, 7)),(-0.35, 0.26)),
		(Set((4, 8)),(0.19, 0.24)), (Set((4, 9)),(-0.34, -0.02)),
		(Set((5, 0)),(-0.13, 0.15)), (Set((5, 1)),(-0.11, 0.05)),
		(Set((5, 2)),(0.15, 0.23)), (Set((5, 3)),(-0.18, 0.16)),
		(Set((5, 4)),(0.16, 0.27)), (Set((5, 5)),(-0.08, 0.05)),
		(Set((5, 6)),(-0.11, 0.0)), (Set((5, 7)),(0.03, 0.22)),
		(Set((5, 8)),(0.12, 0.16)), (Set((5, 9)),(-0.06, 0.27)),
		(Set((6, 0)),(-0.1, 0.16)), (Set((6, 1)),(-0.24, -0.17)),
		(Set((6, 2)),(0.21, 0.31)), (Set((6, 3)),(-0.28, 0.15)),
		(Set((6, 4)),(-0.27, 0.12)), (Set((6, 5)),(0.04, 0.28)),
		(Set((6, 6)),(-0.04, 0.02)), (Set((6, 7)),(-0.27, 0.27)),
		(Set((6, 8)),(-0.01, 0.16)), (Set((6, 9)),(0.05, 0.24)),
		(Set((7, 0)),(0.18, 0.19)), (Set((7, 1)),(-0.35, 0.29)),
		(Set((7, 2)),(-0.34, -0.33)), (Set((7, 3)),(-0.24, 0.21)),
		(Set((7, 4)),(-0.08, 0.21)), (Set((7, 5)),(-0.24, 0.16)),
		(Set((7, 6)),(0.04, 0.22)), (Set((7, 7)),(-0.37, -0.06)),
		(Set((7, 8)),(-0.1, -0.06)), (Set((7, 9)),(-0.25, -0.18)),
		(Set((8, 0)),(-0.3, -0.08)), (Set((8, 1)),(-0.26, 0.15)),
		(Set((8, 2)),(-0.33, 0.2)), (Set((8, 3)),(-0.35, 0.22)),
		(Set((8, 4)),(-0.07, 0.31)), (Set((8, 5)),(-0.35, -0.05)),
		(Set((8, 6)),(0.29, 0.319683)), (Set((8, 7)),(0.19, 0.25)),
		(Set((8, 8)),(-0.22, -0.15)), (Set((8, 9)),(-0.26, -0.09)),
		(Set((9, 0)),(0.01, 0.25)), (Set((9, 1)),(-0.15, 0.06)),
		(Set((9, 2)),(-0.06, 0.01)), (Set((9, 3)),(-0.0, 0.07)),
		(Set((9, 4)),(0.07, 0.09)), (Set((9, 5)),(-0.33, -0.3)),
		(Set((9, 6)),(-0.24, 0.05)), (Set((9, 7)),(-0.24, 0.04)),
		(Set((9, 8)),(-0.24, 0.28)), (Set((9, 9)),(-0.33, 0.28))))
	 && weights2 >= -0.426394 && weights2 <= 0.323056 && weights2.size(10,10)
	 && weights2.specM(Set((Set((0, 0)),(0.17, 0.3)), (Set((0, 1)),(0.13, 0.2)),
		(Set((0, 2)),(-0.34, -0.08)), (Set((0, 3)),(-0.32, 0.32)),
		(Set((0, 4)),(-0.22, -0.2)), (Set((0, 5)),(-0.26, -0.05)),
		(Set((0, 6)),(-0.25, 0.12)), (Set((0, 7)),(-0.34, -0.29)),
		(Set((0, 8)),(-0.35, -0.03)), (Set((0, 9)),(-0.41, 0.26)),
		(Set((1, 0)),(-0.41, 0.01)), (Set((1, 1)),(-0.38, 0.11)),
		(Set((1, 2)),(0.04, 0.12)), (Set((1, 3)),(-0.34, -0.04)),
		(Set((1, 4)),(-0.18, 0.21)), (Set((1, 5)),(-0.01, 0.06)),
		(Set((1, 6)),(-0.18, -0.0)), (Set((1, 7)),(-0.16, 0.24)),
		(Set((1, 8)),(0.18, 0.26)), (Set((1, 9)),(-0.21, -0.01)),
		(Set((2, 0)),(-0.13, 0.12)), (Set((2, 1)),(-0.41, 0.07)),
		(Set((2, 2)),(-0.22, 0.29)), (Set((2, 3)),(-0.31, -0.26)),
		(Set((2, 4)),(0.18, 0.27)), (Set((2, 5)),(-0.22, -0.01)),
		(Set((2, 6)),(-0.28, 0.18)), (Set((2, 7)),(-0.07, 0.32)),
		(Set((2, 8)),(-0.22, 0.05)), (Set((2, 9)),(-0.06, 0.15)),
		(Set((3, 0)),(-0.38, -0.3)), (Set((3, 1)),(-0.02, 0.25)),
		(Set((3, 2)),(-0.05, 0.3)), (Set((3, 3)),(-0.21, 0.31)),
		(Set((3, 4)),(-0.26, -0.2)), (Set((3, 5)),(-0.3, -0.16)),
		(Set((3, 6)),(-0.38, -0.19)), (Set((3, 7)),(-0.02, 0.01)),
		(Set((3, 8)),(0.27, 0.32)), (Set((3, 9)),(0.17, 0.28)),
		(Set((4, 0)),(-0.15, 0.13)), (Set((4, 1)),(-0.23, -0.13)),
		(Set((4, 2)),(-0.34, -0.13)), (Set((4, 3)),(-0.01, 0.03)),
		(Set((4, 4)),(0.18, 0.2)), (Set((4, 5)),(0.16, 0.32)),
		(Set((4, 6)),(-0.16, -0.07)), (Set((4, 7)),(-0.38, -0.36)),
		(Set((4, 8)),(-0.25, -0.01)), (Set((4, 9)),(-0.39, 0.15)),
		(Set((5, 0)),(-0.02, 0.31)), (Set((5, 1)),(0.04, 0.19)),
		(Set((5, 2)),(-0.2, 0.26)), (Set((5, 3)),(0.01, 0.21)),
		(Set((5, 4)),(-0.24, -0.2)), (Set((5, 5)),(-0.39, -0.34)),
		(Set((5, 6)),(-0.23, 0.24)), (Set((5, 7)),(0.02, 0.06)),
		(Set((5, 8)),(-0.42, 0.21)), (Set((5, 9)),(-0.12, 0.14)),
		(Set((6, 0)),(-0.25, 0.07)), (Set((6, 1)),(0.01, 0.03)),
		(Set((6, 2)),(-0.36, 0.26)), (Set((6, 3)),(-0.08, 0.31)),
		(Set((6, 4)),(0.1, 0.12)), (Set((6, 5)),(-0.22, -0.2)),
		(Set((6, 6)),(0.06, 0.22)), (Set((6, 7)),(-0.23, 0.09)),
		(Set((6, 8)),(-0.28, -0.27)), (Set((6, 9)),(-0.42, -0.19)),
		(Set((7, 0)),(-0.29, -0.1)), (Set((7, 1)),(-0.15, 0.19)),
		(Set((7, 2)),(0.15, 0.26)), (Set((7, 3)),(-0.39, -0.23)),
		(Set((7, 4)),(0.01, 0.1)), (Set((7, 5)),(-0.0, 0.17)),
		(Set((7, 6)),(-0.39, 0.25)), (Set((7, 7)),(-0.15, 0.06)),
		(Set((7, 8)),(-0.22, -0.1)), (Set((7, 9)),(-0.11, 0.09)),
		(Set((8, 0)),(-0.15, 0.05)), (Set((8, 1)),(-0.04, 0.29)),
		(Set((8, 2)),(0.04, 0.22)), (Set((8, 3)),(-0.42, 0.18)),
		(Set((8, 4)),(0.21, 0.26)), (Set((8, 5)),(-0.37, -0.29)),
		(Set((8, 6)),(-0.33, 0.21)), (Set((8, 7)),(0.19, 0.32)),
		(Set((8, 8)),(-0.22, 0.17)), (Set((8, 9)),(-0.23, 0.13)),
		(Set((9, 0)),(-0.35, 0.14)), (Set((9, 1)),(-0.42, 0.08)),
		(Set((9, 2)),(-0.32, 0.04)), (Set((9, 3)),(-0.42, -0.01)),
		(Set((9, 4)),(-0.15, 0.0)), (Set((9, 5)),(-0.19, 0.13)),
		(Set((9, 6)),(-0.2, 0.03)), (Set((9, 7)),(-0.19, 0.26)),
		(Set((9, 8)),(-0.3, -0.09)), (Set((9, 9)),(0.01, 0.27))))
	 && weights3 >= -0.582338 && weights3 <= 0.566423 && weights3.size(10,10)
	 && weights3.specM(Set((Set((0, 0)),(-0.07, 0.23)), (Set((0, 1)),(-0.29, 0.54)),
		(Set((0, 2)),(0.2, 0.22)), (Set((0, 3)),(-0.02, 0.49)),
		(Set((0, 4)),(-0.08, 0.41)), (Set((0, 5)),(0.31, 0.51)),
		(Set((0, 6)),(0.1, 0.33)), (Set((0, 7)),(-0.48, 0.2)),
		(Set((0, 8)),(-0.49, 0.55)), (Set((0, 9)),(-0.07, -0.05)),
		(Set((1, 0)),(-0.42, -0.28)), (Set((1, 1)),(-0.24, 0.29)),
		(Set((1, 2)),(-0.14, 0.23)), (Set((1, 3)),(-0.55, -0.08)),
		(Set((1, 4)),(-0.28, -0.09)), (Set((1, 5)),(-0.23, -0.03)),
		(Set((1, 6)),(-0.33, -0.32)), (Set((1, 7)),(0.38, 0.55)),
		(Set((1, 8)),(-0.53, 0.44)), (Set((1, 9)),(-0.23, 0.06)),
		(Set((2, 0)),(-0.58, 0.49)), (Set((2, 1)),(-0.5, -0.27)),
		(Set((2, 2)),(0.24, 0.43)), (Set((2, 3)),(0.02, 0.16)),
		(Set((2, 4)),(-0.54, -0.22)), (Set((2, 5)),(-0.45, -0.0)),
		(Set((2, 6)),(-0.49, 0.15)), (Set((2, 7)),(-0.06, 0.03)),
		(Set((2, 8)),(-0.58, -0.05)), (Set((2, 9)),(-0.37, 0.39)),
		(Set((3, 0)),(-0.21, 0.5)), (Set((3, 1)),(-0.27, 0.14)),
		(Set((3, 2)),(-0.27, 0.18)), (Set((3, 3)),(0.04, 0.34)),
		(Set((3, 4)),(-0.36, -0.23)), (Set((3, 5)),(-0.4, 0.19)),
		(Set((3, 6)),(-0.32, 0.36)), (Set((3, 7)),(0.29, 0.47)),
		(Set((3, 8)),(-0.22, 0.07)), (Set((3, 9)),(-0.55, 0.46)),
		(Set((4, 0)),(-0.18, -0.1)), (Set((4, 1)),(-0.58, -0.07)),
		(Set((4, 2)),(-0.41, -0.17)), (Set((4, 3)),(-0.58, 0.4)),
		(Set((4, 4)),(-0.17, -0.11)), (Set((4, 5)),(-0.38, 0.19)),
		(Set((4, 6)),(-0.54, -0.39)), (Set((4, 7)),(-0.08, 0.02)),
		(Set((4, 8)),(-0.29, -0.25)), (Set((4, 9)),(0.43, 0.46)),
		(Set((5, 0)),(-0.18, 0.25)), (Set((5, 1)),(0.37, 0.37)),
		(Set((5, 2)),(0.17, 0.27)), (Set((5, 3)),(-0.58, -0.53)),
		(Set((5, 4)),(-0.17, 0.47)), (Set((5, 5)),(-0.47, -0.46)),
		(Set((5, 6)),(-0.27, 0.09)), (Set((5, 7)),(-0.47, -0.45)),
		(Set((5, 8)),(-0.4, 0.48)), (Set((5, 9)),(0.32, 0.37)),
		(Set((6, 0)),(-0.58, 0.12)), (Set((6, 1)),(-0.34, 0.45)),
		(Set((6, 2)),(-0.51, 0.55)), (Set((6, 3)),(-0.04, 0.35)),
		(Set((6, 4)),(-0.39, 0.54)), (Set((6, 5)),(-0.44, -0.02)),
		(Set((6, 6)),(-0.24, 0.36)), (Set((6, 7)),(-0.18, 0.24)),
		(Set((6, 8)),(-0.52, 0.08)), (Set((6, 9)),(-0.09, -0.06)),
		(Set((7, 0)),(-0.41, 0.16)), (Set((7, 1)),(-0.16, 0.03)),
		(Set((7, 2)),(-0.25, -0.09)), (Set((7, 3)),(-0.36, 0.51)),
		(Set((7, 4)),(-0.57, -0.23)), (Set((7, 5)),(-0.58, -0.42)),
		(Set((7, 6)),(-0.08, 0.2)), (Set((7, 7)),(-0.46, 0.42)),
		(Set((7, 8)),(-0.51, -0.08)), (Set((7, 9)),(-0.52, -0.08)),
		(Set((8, 0)),(-0.54, 0.31)), (Set((8, 1)),(-0.18, 0.03)),
		(Set((8, 2)),(-0.31, 0.31)), (Set((8, 3)),(-0.13, 0.56)),
		(Set((8, 4)),(-0.25, 0.24)), (Set((8, 5)),(-0.24, -0.03)),
		(Set((8, 6)),(-0.28, 0.11)), (Set((8, 7)),(-0.31, 0.02)),
		(Set((8, 8)),(-0.56, 0.55)), (Set((8, 9)),(0.28, 0.4)),
		(Set((9, 0)),(-0.34, 0.25)), (Set((9, 1)),(-0.38, 0.27)),
		(Set((9, 2)),(0.2, 0.34)), (Set((9, 3)),(-0.58, 0.01)),
		(Set((9, 4)),(-0.31, -0.24)), (Set((9, 5)),(-0.54, -0.07)),
		(Set((9, 6)),(-0.08, 0.18)), (Set((9, 7)),(-0.33, 0.15)),
		(Set((9, 8)),(-0.13, 0.09)), (Set((9, 9)),(-0.19, -0.1))))
	 && weights4 >= -0.293298 && weights4 <= 0.311236 && weights4.size(1,10)
	 && weights4.specM(Set((Set((0, 0)),(-0.22, 0.15)), (Set((0, 1)),(-0.24, 0.06)),
		(Set((0, 2)),(-0.19, 0.04)), (Set((0, 3)),(-0.17, 0.18)),
		(Set((0, 4)),(-0.27, -0.08)), (Set((0, 5)),(-0.04, 0.21)),
		(Set((0, 6)),(-0.02, 0.15)), (Set((0, 7)),(-0.08, -0.01)),
		(Set((0, 8)),(-0.26, 0.18)), (Set((0, 9)),(0.16, 0.2)) ))
	)

    val layer1 = (weights1.x(x) + bias1).map(el => {
      val relu = Vector(List(el, 0.0))
      relu.max()
    })
    val layer2 = (weights2.x(layer1) + bias2).map(el => {
      val relu = Vector(List(el, 0.0))
      relu.max()
    })
    val layer3 = (weights3.x(layer2) + bias3).map(el => {
      val relu = Vector(List(el, 0.0))
      relu.max()
    })
    val layer4 = (weights4.x(layer3) + bias4)

    layer4
  }


}