import daisy.lang._
import Real._
import daisy.lang.Vector._

object MinvMatrix {

	def minvMatrix(
        qpos: Vector
    ): Real = {
        require(
            qpos >= 0.2 && qpos <= 0.6 && qpos.size(6) &&
            qpos.specV(Set(
                ((0, 0), (0.2, 0.6)),
                ((1, 1), (0.2, 0.6)),
                ((2, 2), (0.2, 0.6)),
                ((3, 3), (0.2, 0.6)),
                ((4, 4), (0.2, 0.6)),
                ((5, 5), (0.2, 0.6))
            ))
        )
        val sin_qpos = qpos.sin()
        val cos_qpos = qpos.cos()

        // val my_temp1 = -sin_qpos.at(2)

        // val kh: Matrix = Matrix(List(List(-1, 0, 1), List(-2, 0, 2), List(-1, 0, 1)))
        // val direction: Vector = Vector(List(0.0, -2.0))
        val rotation_matrix_0: Matrix = Matrix(List(
            List(cos_qpos.at(0), -sin_qpos.at(0), 0.0),
            List(sin_qpos.at(0), cos_qpos.at(0), 0.0),
            List(0.0, 0.0, 1.0)
        ))

        val limi_rotation_0: Matrix = Matrix(List(
            List(rotation_matrix_0.at(0, 0), rotation_matrix_0.at(0, 1), 0.0),
            List(rotation_matrix_0.at(1, 0), rotation_matrix_0.at(1, 1), 0.0),
            List(0.0, 0.0, 1.0)
        ))

        val oMi_rotation_0: Matrix = limi_rotation_0

        val rotation_matrix_1: Matrix = Matrix(List(
            List(cos_qpos.at(1), -sin_qpos.at(1), 0.0),
            List(sin_qpos.at(1), cos_qpos.at(1), 0.0),
            List(0.0, 0.0, 1.0)
        ))

        val limi_rotation_1: Matrix = Matrix(List(
            List(rotation_matrix_1.at(0, 0), rotation_matrix_1.at(0, 1), 0.0),
            List(0.0, 0.0, 1.0),
            List(-rotation_matrix_1.at(1, 0), -rotation_matrix_1.at(1, 1), 0.0)
        ))

        val oMi_rotation_1: Matrix = oMi_rotation_0 * (limi_rotation_1)

        limi_rotation_1.at(0, 0)
    }

}