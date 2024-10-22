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
        val oMi_translation_5: Vector = oMi_translation_4

        // pass 2
        // JOINT 5
        val Ia_5_pred = Matrix( List(
            List(2.40208, 0.0, 0.0, -0.0, -0.0206546, 0.0688327),
            List(0.0, 2.40208, 0.0, 0.0206546, -0.0, 0.172703),
            List(0.0, 0.0, 2.40208, -0.0688327, -0.172703, -0.0),
            List(0.0, 0.0206546, -0.0688327, 0.0178005, 0.00718352, -0.000223653),
            List(-0.0206546, 0.0, -0.172703, 0.00718352, 0.0225347, 0.000641928),
            List(0.0688327, 0.172703, 0.0, -0.000223653, 0.000641928, 0.031751)
        ))
        val U_5 = Matrix( List( List(0.0688327), List(0.172703), List(0.0), List(-0.000223653), List(0.000641928), List(0.031751)))
        val Dinv_5 = 1.0 / Ia_5_pred.at(5, 5)
        val UDinv_5: Matrix = U_5 * Dinv_5
        val U_5_t = U_5.transpose()
        val Ia_5_minus: Matrix = UDinv_5.x(U_5_t)
        val Ia_5 = Ia_5_pred - Ia_5_minus

        // ActOn

        // Ao is a 3x3 matrix, the upperside of Ia_5
        val Ai_4 = Ia_5.slice(0, 0)(3, 3)
        val Bi_4 = Ia_5.slice(0, 3)(3, 3)
        val Ci_4 = Ia_5.slice(3, 0)(3, 3)
        val Di_4 = Ia_5.slice(3, 3)(3, 3)

        val limi_rotation_5_t = limi_rotation_5.transpose()

        val Ao_4_temp1 = limi_rotation_5.x(Ai_4)
        val Ao_4 = Ao_4_temp1.x(limi_rotation_5_t)
        val Bo_4_temp1 = limi_rotation_5.x(Bi_4)
        val Bo_4_temp = Bo_4_temp1.x(limi_rotation_5_t)
        val Do_4_temp1 = limi_rotation_5.x(Di_4)
        val Do_4_temp = Do_4_temp1.x(limi_rotation_5_t)

        val Bo_4_temp_t = Bo_4_temp.transpose()
        val Bo_4_temp_col1: Vector = Bo_4_temp_t.row(0)
        val Bo_4_temp_col2: Vector = Bo_4_temp_t.row(1)
        val Bo_4_temp_col3: Vector = Bo_4_temp_t.row(2)

        val Do_4_temp_row1 = Do_4_temp.row(0)
        val Do_4_temp_row2 = Do_4_temp.row(1)
        val Do_4_temp_row3 = Do_4_temp.row(2)

        val Do_4_temp_row1_new1 = limi_translation_5.x(Bo_4_temp_col1)
        val Do_4_temp_row1_new = Do_4_temp_row1 + Do_4_temp_row1_new1
        val Do_4_temp_row2_new1 = limi_translation_5.x(Bo_4_temp_col2)
        val Do_4_temp_row2_new = Do_4_temp_row2 + Do_4_temp_row2_new1
        val Do_4_temp_row3_new1 = limi_translation_5.x(Bo_4_temp_col3)
        val Do_4_temp_row3_new = Do_4_temp_row3 + Do_4_temp_row3_new1

        val Ao_4_t = Ao_4.transpose()
        val Ao_4_col1: Vector = Ao_4_t.row(0)
        val Ao_4_col2: Vector = Ao_4_t.row(1)
        val Ao_4_col3: Vector = Ao_4_t.row(2)
        
        val Co_4_temp_col1 = limi_translation_5.x(Ao_4_col1)
        val Co_4_temp_col2 = limi_translation_5.x(Ao_4_col2)
        val Co_4_temp_col3 = limi_translation_5.x(Ao_4_col3)

        // Cos appended and Bo_t added makes final Co
        val Co_4_temp2 = Matrix(List(
            List(Co_4_temp_col1.at(0), Co_4_temp_col2.at(0), Co_4_temp_col3.at(0)),
            List(Co_4_temp_col1.at(1), Co_4_temp_col2.at(1), Co_4_temp_col3.at(1)),
            List(Co_4_temp_col1.at(2), Co_4_temp_col2.at(2), Co_4_temp_col3.at(2))
        ))
        val Co_4_temp3 = Co_4_temp2.transpose()
        val Co_4 = Co_4_temp3 + Bo_4_temp_t

        val Bo_4 = Co_4.transpose()

        val Do_temp2_4 = Matrix(List(
            List(Do_4_temp_row1_new.at(0), Do_4_temp_row1_new.at(1), Do_4_temp_row1_new.at(2)),
            List(Do_4_temp_row2_new.at(0), Do_4_temp_row2_new.at(1), Do_4_temp_row2_new.at(2)),
            List(Do_4_temp_row3_new.at(0), Do_4_temp_row3_new.at(1), Do_4_temp_row3_new.at(2))
        ))

        val Do_4_temp2_t = Do_temp2_4.transpose()
        val Do_4_temp2_col1: Vector = Do_4_temp2_t.row(0)
        val Do_4_temp2_col2: Vector = Do_4_temp2_t.row(1)
        val Do_4_temp2_col3: Vector = Do_4_temp2_t.row(2)

        val Co_4_col1 = Bo_4.row(0)
        val Co_4_col2 = Co_4.row(1)
        val Co_4_col3 = Co_4.row(2)

        val Do_4_temp3_col1 = limi_translation_5.x(Co_4_col1)
        val Do_4_temp3_col2 = limi_translation_5.x(Co_4_col2)
        val Do_4_temp3_col3 = limi_translation_5.x(Co_4_col3)

        val Do_4_temp4_col1 = Do_4_temp2_col1 + Do_4_temp3_col1
        val Do_4_temp4_col2 = Do_4_temp2_col2 + Do_4_temp3_col2
        val Do_4_temp4_col3 = Do_4_temp2_col3 + Do_4_temp3_col3

        val Do_4 = Matrix(List(
            List(Do_4_temp4_col1.at(0), Do_4_temp4_col2.at(0), Do_4_temp4_col3.at(0)),
            List(Do_4_temp4_col1.at(1), Do_4_temp4_col2.at(1), Do_4_temp4_col3.at(1)),
            List(Do_4_temp4_col1.at(2), Do_4_temp4_col2.at(2), Do_4_temp4_col3.at(2))
        ))

        val Ia_4_add = Matrix(List(
            List(Ao_4.at(0, 0), Ao_4.at(0, 1), Ao_4.at(0, 2), Bo_4.at(0, 0), Bo_4.at(0, 1), Bo_4.at(0, 2)),
            List(Ao_4.at(1, 0), Ao_4.at(1, 1), Ao_4.at(1, 2), Bo_4.at(1, 0), Bo_4.at(1, 1), Bo_4.at(1, 2)),
            List(Ao_4.at(2, 0), Ao_4.at(2, 1), Ao_4.at(2, 2), Bo_4.at(2, 0), Bo_4.at(2, 1), Bo_4.at(2, 2)),
            List(Co_4.at(0, 0), Co_4.at(0, 1), Co_4.at(0, 2), Do_4.at(0, 0), Do_4.at(0, 1), Do_4.at(0, 2)),
            List(Co_4.at(1, 0), Co_4.at(1, 1), Co_4.at(1, 2), Do_4.at(1, 0), Do_4.at(1, 1), Do_4.at(1, 2)),
            List(Co_4.at(2, 0), Co_4.at(2, 1), Co_4.at(2, 2), Do_4.at(2, 0), Do_4.at(2, 1), Do_4.at(2, 2))
        ))

        val Ia_4_predec = Matrix(List(
            List(1.22595, 0.0, 0.0, -0.0, -0.0471217, -0.0503435),
            List(0.0, 1.22595, 0.0, 0.0471217, -0.0, -0.0146537),
            List(0.0, 0.0, 1.22595, 0.0503435, 0.0146537, -0.0),
            List(0.0, 0.0471217, 0.0503435, 0.0394276, -0.00151524, -0.00460025),
            List(-0.0471217, 0.0, 0.0146537, -0.00151524, 0.0314604, 0.00216405),
            List(-0.0503435, -0.0146537, 0.0, -0.00460025, 0.00216405, 0.0108695)
        ))

        val Ia_4 = Ia_4_predec + Ia_4_add

        // JOINT 4


        Dinv_5
    } ensuring(res => res +/- 1e-5)

}

