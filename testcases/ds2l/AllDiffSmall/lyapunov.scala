import daisy.lang._
import Real._
import daisy.lang.Vector._

object lyapunov {

	def lyapunov(x: Vector, weights1: Matrix, weights2: Matrix, bias1: Vector, bias2: Real): Vector = {
require(0.5307131 <= bias2 && bias2 <= 0.5307131
	 && x >= -6.0 && x <= 6.0 && x.size(10)
	 && x.specV(Set(((0, 0),(-3.02, 4.06)), ((1, 1),(0.89, 2.35)), ((2, 2),(-0.94, -0.23)),
((3, 3),(-1.61, 2.75)), ((4, 4),(-3.99, 0.05)), ((5, 5),(-5.02, 2.76)),
((6, 6),(-4.38, 1.69)), ((7, 7),(-5.02, 2.43)), ((8, 8),(-1.53, 5.56)),
((9, 9),(-5.11, 1.67))))
	 && bias1 >= -0.8746956 && bias1 <= 1.1860801 && bias1.size(10)
	 && bias1.specV(Set(((0, 0),(-0.46, 0.31)), ((1, 1),(-0.22, 0.89)), ((2, 2),(-0.62, 0.24)),
((3, 3),(-0.32, 0.12)), ((4, 4),(-0.32, 0.95)), ((5, 5),(0.28, 0.43)),
((6, 6),(0.95, 1.06)), ((7, 7),(-0.82, -0.19)), ((8, 8),(0.15, 1.01)),
((9, 9),(-0.81, -0.24))))
	 && weights1 >= -0.6363012 && weights1 <= 1.0211772 && weights1.size(10,10)
	 && weights1.specM(Set((Set((0, 0)),(0.03, 0.07)), (Set((0, 1)),(0.27, 0.41)),
		(Set((0, 2)),(-0.56, -0.55)), (Set((0, 3)),(-0.54, 0.16)),
		(Set((0, 4)),(-0.59, 0.86)), (Set((0, 5)),(-0.08, 0.81)),
		(Set((0, 6)),(-0.49, 0.8)), (Set((0, 7)),(0.22, 0.26)),
		(Set((0, 8)),(-0.01, 0.65)), (Set((0, 9)),(0.8, 1.01)),
		(Set((1, 0)),(0.13, 0.49)), (Set((1, 1)),(0.86, 0.92)),
		(Set((1, 2)),(-0.49, -0.03)), (Set((1, 3)),(-0.03, 0.09)),
		(Set((1, 4)),(-0.08, 0.82)), (Set((1, 5)),(-0.29, -0.26)),
		(Set((1, 6)),(-0.2, 0.54)), (Set((1, 7)),(-0.46, 0.17)),
		(Set((1, 8)),(-0.36, 0.61)), (Set((1, 9)),(-0.56, -0.52)),
		(Set((2, 0)),(-0.27, 0.02)), (Set((2, 1)),(-0.17, 0.92)),
		(Set((2, 2)),(-0.62, -0.1)), (Set((2, 3)),(-0.24, -0.2)),
		(Set((2, 4)),(-0.26, 0.04)), (Set((2, 5)),(-0.27, 0.38)),
		(Set((2, 6)),(-0.42, 0.02)), (Set((2, 7)),(-0.21, 0.01)),
		(Set((2, 8)),(-0.5, 0.83)), (Set((2, 9)),(-0.5, -0.1)),
		(Set((3, 0)),(0.33, 0.88)), (Set((3, 1)),(0.14, 0.69)),
		(Set((3, 2)),(-0.62, 0.74)), (Set((3, 3)),(0.35, 0.92)),
		(Set((3, 4)),(-0.46, 0.75)), (Set((3, 5)),(-0.31, 0.34)),
		(Set((3, 6)),(0.52, 0.57)), (Set((3, 7)),(-0.26, 0.34)),
		(Set((3, 8)),(-0.48, 0.48)), (Set((3, 9)),(0.14, 0.6)),
		(Set((4, 0)),(0.41, 0.95)), (Set((4, 1)),(0.03, 0.4)),
		(Set((4, 2)),(0.49, 0.51)), (Set((4, 3)),(-0.21, 0.13)),
		(Set((4, 4)),(0.42, 0.68)), (Set((4, 5)),(0.72, 0.84)),
		(Set((4, 6)),(-0.03, 0.92)), (Set((4, 7)),(-0.12, 0.44)),
		(Set((4, 8)),(0.19, 0.89)), (Set((4, 9)),(-0.05, 0.54)),
		(Set((5, 0)),(0.05, 0.92)), (Set((5, 1)),(0.1, 0.91)),
		(Set((5, 2)),(-0.25, 0.67)), (Set((5, 3)),(0.08, 0.93)),
		(Set((5, 4)),(-0.07, 0.33)), (Set((5, 5)),(-0.17, 0.24)),
		(Set((5, 6)),(-0.22, 0.62)), (Set((5, 7)),(-0.08, 0.91)),
		(Set((5, 8)),(-0.1, 0.0)), (Set((5, 9)),(-0.5, -0.26)),
		(Set((6, 0)),(-0.46, 0.8)), (Set((6, 1)),(-0.59, 0.56)),
		(Set((6, 2)),(-0.45, 0.8)), (Set((6, 3)),(0.32, 0.46)),
		(Set((6, 4)),(-0.52, 0.65)), (Set((6, 5)),(0.64, 0.77)),
		(Set((6, 6)),(0.16, 0.66)), (Set((6, 7)),(0.79, 0.92)),
		(Set((6, 8)),(0.22, 0.4)), (Set((6, 9)),(-0.41, 0.23)),
		(Set((7, 0)),(-0.18, 0.84)), (Set((7, 1)),(-0.33, 0.97)),
		(Set((7, 2)),(0.15, 0.88)), (Set((7, 3)),(-0.0, 0.93)),
		(Set((7, 4)),(0.07, 0.33)), (Set((7, 5)),(0.55, 0.87)),
		(Set((7, 6)),(0.12, 0.78)), (Set((7, 7)),(-0.38, 0.72)),
		(Set((7, 8)),(0.49, 0.68)), (Set((7, 9)),(-0.38, 0.43)),
		(Set((8, 0)),(0.2, 0.74)), (Set((8, 1)),(-0.34, -0.12)),
		(Set((8, 2)),(0.31, 0.8)), (Set((8, 3)),(-0.58, 0.9)),
		(Set((8, 4)),(-0.62, 0.68)), (Set((8, 5)),(0.16, 0.8)),
		(Set((8, 6)),(0.08, 0.56)), (Set((8, 7)),(-0.32, 0.86)),
		(Set((8, 8)),(0.14, 0.64)), (Set((8, 9)),(-0.46, 0.45)),
		(Set((9, 0)),(-0.32, 0.82)), (Set((9, 1)),(-0.34, 0.95)),
		(Set((9, 2)),(0.05, 0.4)), (Set((9, 3)),(-0.26, 0.86)),
		(Set((9, 4)),(0.59, 0.71)), (Set((9, 5)),(0.89, 0.95)),
		(Set((9, 6)),(-0.6, 0.06)), (Set((9, 7)),(-0.19, -0.08)),
		(Set((9, 8)),(-0.51, 1.01)), (Set((9, 9)),(-0.62, 0.51))))
	 && weights2 >= -0.80846876 && weights2 <= 1.1081733 && weights2.size(1,10)
	 && weights2.specM(Set((Set((0, 0)),(-0.55, -0.3)), (Set((0, 1)),(-0.78, -0.45)),
		(Set((0, 2)),(-0.43, -0.3)), (Set((0, 3)),(0.32, 0.78)),
		(Set((0, 4)),(-0.14, 0.57)), (Set((0, 5)),(-0.2, -0.13)),
		(Set((0, 6)),(-0.12, -0.06)), (Set((0, 7)),(-0.38, 0.63)),
		(Set((0, 8)),(0.09, 0.18)), (Set((0, 9)),(-0.25, 0.75)),
		(Set((1, 0)),(0.22, 0.49)), (Set((1, 1)),(-0.46, 1.04)),
		(Set((1, 2)),(-0.12, 0.98)), (Set((1, 3)),(-0.27, 0.29)),
		(Set((1, 4)),(0.48, 0.53)), (Set((1, 5)),(-0.24, 0.36)),
		(Set((1, 6)),(-0.05, 0.55)), (Set((1, 7)),(-0.53, 0.55)),
		(Set((1, 8)),(-0.78, -0.31)), (Set((1, 9)),(0.46, 0.5)),
		(Set((2, 0)),(-0.8, -0.23)), (Set((2, 1)),(-0.69, -0.05)),
		(Set((2, 2)),(-0.68, 0.71)), (Set((2, 3)),(-0.57, 0.75)),
		(Set((2, 4)),(-0.41, 0.27)), (Set((2, 5)),(0.46, 0.91)),
		(Set((2, 6)),(-0.24, -0.02)), (Set((2, 7)),(0.33, 0.8)),
		(Set((2, 8)),(-0.25, 0.67)), (Set((2, 9)),(0.84, 1.05)),
		(Set((3, 0)),(-0.4, 0.05)), (Set((3, 1)),(0.33, 0.4)),
		(Set((3, 2)),(0.22, 0.67)), (Set((3, 3)),(0.09, 1.08)),
		(Set((3, 4)),(0.54, 0.71)), (Set((3, 5)),(0.22, 0.77)),
		(Set((3, 6)),(-0.6, -0.1)), (Set((3, 7)),(-0.41, 0.36)),
		(Set((3, 8)),(-0.7, 0.31)), (Set((3, 9)),(-0.04, 0.41)),
		(Set((4, 0)),(-0.07, 0.88)), (Set((4, 1)),(-0.22, 0.62)),
		(Set((4, 2)),(0.88, 0.98)), (Set((4, 3)),(-0.1, 0.44)),
		(Set((4, 4)),(0.32, 0.73)), (Set((4, 5)),(-0.26, 1.05)),
		(Set((4, 6)),(0.83, 0.88)), (Set((4, 7)),(-0.45, 0.98)),
		(Set((4, 8)),(-0.80846876, 0.57)), (Set((4, 9)),(0.04, 0.1)),
		(Set((5, 0)),(0.86, 1.08)), (Set((5, 1)),(0.87, 0.91)),
		(Set((5, 2)),(-0.68, -0.19)), (Set((5, 3)),(-0.55, 0.8)),
		(Set((5, 4)),(-0.16, 1.0)), (Set((5, 5)),(-0.36, -0.2)),
		(Set((5, 6)),(-0.63, -0.61)), (Set((5, 7)),(-0.37, 0.8)),
		(Set((5, 8)),(-0.77, 0.6)), (Set((5, 9)),(-0.58, -0.19)),
		(Set((6, 0)),(0.2, 0.69)), (Set((6, 1)),(-0.76, 0.06)),
		(Set((6, 2)),(-0.13, 0.31)), (Set((6, 3)),(-0.6, -0.13)),
		(Set((6, 4)),(-0.12, 0.48)), (Set((6, 5)),(-0.39, 0.65)),
		(Set((6, 6)),(-0.65, 0.9)), (Set((6, 7)),(-0.8, 0.8)),
		(Set((6, 8)),(0.71, 0.86)), (Set((6, 9)),(-0.23, 0.76)),
		(Set((7, 0)),(-0.66, -0.12)), (Set((7, 1)),(0.76, 1.1)),
		(Set((7, 2)),(-0.61, -0.02)), (Set((7, 3)),(-0.12, 0.01)),
		(Set((7, 4)),(-0.55, 0.01)), (Set((7, 5)),(-0.35, 0.84)),
		(Set((7, 6)),(0.01, 0.55)), (Set((7, 7)),(-0.33, 0.08)),
		(Set((7, 8)),(0.78, 1.03)), (Set((7, 9)),(-0.63, 0.91)),
		(Set((8, 0)),(0.27, 1.1)), (Set((8, 1)),(0.29, 0.61)),
		(Set((8, 2)),(-0.69, 0.62)), (Set((8, 3)),(0.69, 0.73)),
		(Set((8, 4)),(0.61, 0.74)), (Set((8, 5)),(-0.52, -0.28)),
		(Set((8, 6)),(-0.53, 0.76)), (Set((8, 7)),(0.08, 0.24)),
		(Set((8, 8)),(-0.77, -0.45)), (Set((8, 9)),(-0.5, 0.97)),
		(Set((9, 0)),(-0.47, 0.25)), (Set((9, 1)),(-0.03, 1.09)),
		(Set((9, 2)),(-0.8, -0.58)), (Set((9, 3)),(0.05, 0.37)),
		(Set((9, 4)),(-0.77, -0.65)), (Set((9, 5)),(-0.52, -0.15)),
		(Set((9, 6)),(0.67, 0.83)), (Set((9, 7)),(-0.42, -0.36)),
		(Set((9, 8)),(0.55, 1.05)), (Set((9, 9)),(-0.54, 0.77))))
	)

    val layer1: Vector = (weights1.x(x) + bias1).map(el => {
      val relu = Vector(List(el, 0.0))
      relu.max()
    })
    val layer2: Vector = (weights2.x(layer1) + bias2).map(el => {
      val relu = Vector(List(el, 0.0))
      relu.max()
    })
    layer2
  }


}