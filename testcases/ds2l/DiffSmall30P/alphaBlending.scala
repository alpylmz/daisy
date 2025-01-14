import daisy.lang._
import Real._
import daisy.lang.Vector._

object alphaBlending {

	def alphaBlending(b: Matrix, c: Matrix, alpha: Real): Matrix = {
require(0.0 <= alpha && alpha <= 1.0
	 && b >= 223.35 && b <= 530.05 && b.size(10,10)
	 && b.specM(Set((Set((4, 0), (4, 3), (9, 5), (0, 2)),(281.81, 525.35)), (Set((0, 1), (0, 7), (2, 4), (3, 4)),(510.5, 523.34)),
		(Set((4, 9), (3, 7), (7, 0), (1, 8), (8, 0)),(228.72, 364.12))))
	 && c >= -253.26 && c <= -108.41 && c.size(10,10)
	 && c.specM(Set((Set((3, 4), (3, 7), (4, 6), (5, 1), (5, 7), (9, 5), (8, 9), (8, 6), (1, 0), (1, 6), (2, 5), (1, 3), (1, 9), (2, 8), (7, 1), (7, 7), (4, 2), (4, 5), (5, 0), (5, 6), (3, 9), (5, 9), (8, 2), (0, 4), (2, 1), (1, 5), (7, 3), (4, 7), (5, 2), (9, 0), (5, 5), (9, 3), (8, 1), (8, 7), (1, 1), (0, 3), (2, 0), (0, 6), (2, 9)),(-158.05, -111.26)), (Set((4, 3), (3, 1), (5, 4), (8, 0), (0, 2), (0, 5), (2, 2), (0, 8)),(-215.84, -170.75)),
		(Set((7, 4), (6, 2), (6, 5), (6, 8), (4, 8), (5, 3), (9, 1), (9, 7), (2, 4), (1, 2), (6, 4), (6, 7)),(-144.67, -124.74))))
	)

          b * alpha + c * (1 - alpha)
  }
          


}