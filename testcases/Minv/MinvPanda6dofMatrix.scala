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

        // JOINT 0
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
        val limi_translation_0 = Vector(
            List(0.0, 0.0, 0.333)
        )

        val oMi_rotation_0: Matrix = limi_rotation_0
        val oMi_translation_0 = limi_translation_0

        // JOINT 1

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
        val limi_translation_1 = Vector(
            List(0.0, 0.0, 0.0)
        )

        val oMi_rotation_1: Matrix = oMi_rotation_0.x(limi_rotation_1)
        val oMi_translation_1: Vector = limi_translation_1

        // JOINT 2

        val rotation_matrix_2 = Matrix(List(
            List(cos_qpos.at(2), -sin_qpos.at(2), 0.0),
            List(sin_qpos.at(2), cos_qpos.at(2), 0.0),
            List(0.0, 0.0, 1.0)
        ))

        val limi_rotation_2 = Matrix(List(
            List(rotation_matrix_2.at(0, 0), rotation_matrix_2.at(0, 1), 0.0),
            List(0.0, 0.0, -1.0),
            List(rotation_matrix_2.at(1, 0), rotation_matrix_2.at(1, 1), 0.0)
        ))
        val limi_translation_2 = Vector(
            List(0.0, -0.316, 0.0)
        )

        val oMi_rotation_2 = oMi_rotation_1.x(limi_rotation_2)
        val oMi_translation_cp_2 = oMi_rotation_1.x(limi_translation_2)
        val oMi_translation_2 = oMi_translation_1 + oMi_translation_cp_2

        // JOINT 3

        val rotation_matrix_3 = Matrix(List(
            List(cos_qpos.at(3), -sin_qpos.at(3), 0.0),
            List(sin_qpos.at(3), cos_qpos.at(3), 0.0),
            List(0.0, 0.0, 1.0)
        ))

        val limi_rotation_3 = Matrix(List(
            List(rotation_matrix_3.at(0, 0), rotation_matrix_3.at(0, 1), 0.0),
            List(0.0, 0.0, -1.0),
            List(rotation_matrix_3.at(1, 0), rotation_matrix_3.at(1, 1), 0.0)
        ))
        val limi_translation_3 = Vector(
            List(0.083, 0.0, 0.0)
        )

        val oMi_rotation_3 = oMi_rotation_2.x(limi_rotation_3)
        val oMi_translation_cp_3 = oMi_rotation_2.x(limi_translation_3)
        val oMi_translation_3 = oMi_translation_2 + oMi_translation_cp_3

        // JOINT 4

        val rotation_matrix_4 = Matrix(List(
            List(cos_qpos.at(4), -sin_qpos.at(4), 0.0),
            List(sin_qpos.at(4), cos_qpos.at(4), 0.0),
            List(0.0, 0.0, 1.0)
        ))

        val limi_rotation_4 = Matrix(List(
            List(rotation_matrix_4.at(0, 0), rotation_matrix_4.at(0, 1), 0.0),
            List(0.0, 0.0, 1.0),
            List(-rotation_matrix_4.at(1, 0), -rotation_matrix_4.at(1, 1), 0.0)
        ))
        val limi_translation_4 = Vector(
            List(-0.083, 0.384, 0.0)
        )

        val oMi_rotation_4 = oMi_rotation_3.x(limi_rotation_4)
        val oMi_translation_cp_4 = oMi_rotation_3.x(limi_translation_4)
        val oMi_translation_4 = oMi_translation_3 + oMi_translation_cp_4

        // JOINT 5

        val rotation_matrix_5 = Matrix(List(
            List(cos_qpos.at(5), -sin_qpos.at(5), 0.0),
            List(sin_qpos.at(5), cos_qpos.at(5), 0.0),
            List(0.0, 0.0, 1.0)
        ))

        val limi_rotation_5 = Matrix(List(
            List(rotation_matrix_5.at(0, 0), rotation_matrix_5.at(0, 1), 0.0),
            List(0.0, 0.0, -1.0),
            List(rotation_matrix_5.at(1, 0), rotation_matrix_5.at(1, 1), 0.0)
        ))
        val limi_translation_5 = Vector(
            List(0.0, 0.0, 0.0)
        )

        val oMi_rotation_5 = oMi_rotation_4.x(limi_rotation_5)
        val oMi_translation_5 = oMi_translation_4

        // pass 2
        //         //      [2.40208, 0.0, 0.0, -0.0, -0.0206546, 0.0688327], 
        //      [0.0, 2.40208, 0.0, 0.0206546, -0.0, 0.172703], 
        //      [0.0, 0.0, 2.40208, -0.0688327, -0.172703, -0.0], 
        //      [0.0, 0.0206546, -0.0688327, 0.0178005, 0.00718352, -0.000223653], 
        //      [-0.0206546, 0.0, -0.172703, 0.00718352, 0.0225347, 0.000641928], 
        //      [0.0688327, 0.172703, 0.0, -0.000223653, 0.000641928, 0.031751]]
        val Ia_5_pred = Matrix( List(
            List(2.40208, 0.0, 0.0, -0.0, -0.0206546, 0.0688327),
            List(0.0, 2.40208, 0.0, 0.0206546, -0.0, 0.172703),
            List(0.0, 0.0, 2.40208, -0.0688327, -0.172703, -0.0),
            List(0.0, 0.0206546, -0.0688327, 0.0178005, 0.00718352, -0.000223653),
            List(-0.0206546, 0.0, -0.172703, 0.00718352, 0.0225347, 0.000641928),
            List(0.0688327, 0.172703, 0.0, -0.000223653, 0.000641928, 0.031751)
        ))
        val U_5 = Matrix( List(
            List(0.0688327),
            List(0.172703), 
            List(0.0),
            List(-0.000223653), 
            List(0.000641928), 
            List(0.031751)
        ))
        val U_5_vector: Vector = Ia_5_pred.row(5)

        val Dinv_5 = 1.0 / Ia_5_pred.at(5, 5)

        val UDinv_5: Matrix = U_5 * Dinv_5

        val UDinv_5_t = UDinv_5.transpose()

        val Ia_5_minus: Matrix = UDinv_5_t.x(U_5)
        // need to convert this to matrix
        //val Ia_5_minus_matrix: Matrix = Matrix(
        //    List(Ia_5_minus.toList())
        //)
        
        Dinv_5
    } ensuring(res => res +/- 1e-5)

}

