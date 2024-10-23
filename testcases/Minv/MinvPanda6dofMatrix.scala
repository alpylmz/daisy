import daisy.lang._
import Real._
import daisy.lang.Vector._

object MinvMatrix {

	def minvMatrix(
        qpos: Vector
    ): Real = {
        require(
            qpos >= 0.2 && qpos <= 0.21 && qpos.size(6) &&
            qpos.specV(Set(
                ((0, 0), (0.2, 0.21)),
                ((1, 1), (0.2, 0.21)),
                ((2, 2), (0.2, 0.21)),
                ((3, 3), (0.2, 0.21)),
                ((4, 4), (0.2, 0.21)),
                ((5, 5), (0.2, 0.21))
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
        val UDinv_5 = U_5 * Dinv_5
        val U_5_t = U_5.transpose()
        val Ia_5_minus = UDinv_5.x(U_5_t)
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

        val Do_4_temp2_row1_temp = limi_translation_5.x(Bo_4_temp_col1)
        val Do_4_temp2_row1 = Do_4_temp_row1 + Do_4_temp2_row1_temp
        val Do_4_temp2_row2_temp = limi_translation_5.x(Bo_4_temp_col2)
        val Do_4_temp2_row2 = Do_4_temp_row2 + Do_4_temp2_row2_temp
        val Do_4_temp2_row3_temp = limi_translation_5.x(Bo_4_temp_col3)
        val Do_4_temp2_row3 = Do_4_temp_row3 + Do_4_temp2_row3_temp

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
        val Co_4_temp2_t = Co_4_temp2.transpose()
        val Co_4 = Co_4_temp2 + Bo_4_temp_t

        val Bo_4 = Co_4.transpose()

        val Do_4_temp2 = Matrix(List(
            List(Do_4_temp2_row1.at(0), Do_4_temp2_row1.at(1), Do_4_temp2_row1.at(2)),
            List(Do_4_temp2_row2.at(0), Do_4_temp2_row2.at(1), Do_4_temp2_row2.at(2)),
            List(Do_4_temp2_row3.at(0), Do_4_temp2_row3.at(1), Do_4_temp2_row3.at(2))
        ))

        val Do_4_temp2_t = Do_4_temp2.transpose()
        val Do_4_temp2_col1: Vector = Do_4_temp2_t.row(0)
        val Do_4_temp2_col2: Vector = Do_4_temp2_t.row(1)
        val Do_4_temp2_col3: Vector = Do_4_temp2_t.row(2)

        val Co_4_col1 = Bo_4.row(0)
        val Co_4_col2 = Bo_4.row(1)
        val Co_4_col3 = Bo_4.row(2)

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

        val Ia_4_temp = Ia_4_predec + Ia_4_add

        // JOINT 4
        val U_4 = Matrix(List(
            List(Ia_4_temp.at(5, 0)),
            List(Ia_4_temp.at(5, 1)),
            List(Ia_4_temp.at(5, 2)),
            List(Ia_4_temp.at(5, 3)),
            List(Ia_4_temp.at(5, 4)),
            List(Ia_4_temp.at(5, 5))
        ))
        val Dinv_4 = 1.0 / Ia_4_temp.at(5, 5)
        val UDinv_4 = U_4 * Dinv_4
        val U_4_t = U_4.transpose()
        val Ia_4_minus = UDinv_4.x(U_4_t)
        val Ia_4 = Ia_4_temp - Ia_4_minus

        // ActOn

        val Ai_3 = Ia_4.slice(0, 0)(3, 3)
        val Bi_3 = Ia_4.slice(0, 3)(3, 3)
        val Ci_3 = Ia_4.slice(3, 0)(3, 3)
        val Di_3 = Ia_4.slice(3, 3)(3, 3)

        val limi_rotation_4_t = limi_rotation_4.transpose()

        val Ao_3_temp1 = limi_rotation_4.x(Ai_3)
        val Ao_3 = Ao_3_temp1.x(limi_rotation_4_t)
        val Bo_3_temp1 = limi_rotation_4.x(Bi_3)
        val Bo_3_temp = Bo_3_temp1.x(limi_rotation_4_t)
        val Do_3_temp1 = limi_rotation_4.x(Di_3)
        val Do_3_temp = Do_3_temp1.x(limi_rotation_4_t)

        val Bo_3_temp_t = Bo_3_temp.transpose()
        val Bo_3_temp_col1: Vector = Bo_3_temp_t.row(0)
        val Bo_3_temp_col2: Vector = Bo_3_temp_t.row(1)
        val Bo_3_temp_col3: Vector = Bo_3_temp_t.row(2)

        val Do_3_temp_row1 = Do_3_temp.row(0)
        val Do_3_temp_row2 = Do_3_temp.row(1)
        val Do_3_temp_row3 = Do_3_temp.row(2)

        val Do_3_temp2_row1_temp = limi_translation_4.x(Bo_3_temp_col1)
        val Do_3_temp2_row1 = Do_3_temp_row1 + Do_3_temp2_row1_temp
        val Do_3_temp2_row2_temp = limi_translation_4.x(Bo_3_temp_col2)
        val Do_3_temp2_row2 = Do_3_temp_row2 + Do_3_temp2_row2_temp
        val Do_3_temp2_row3_temp = limi_translation_4.x(Bo_3_temp_col3)
        val Do_3_temp2_row3 = Do_3_temp_row3 + Do_3_temp2_row3_temp

        val Ao_3_t = Ao_3.transpose()
        val Ao_3_col1: Vector = Ao_3_t.row(0)
        val Ao_3_col2: Vector = Ao_3_t.row(1)
        val Ao_3_col3: Vector = Ao_3_t.row(2)

        val Co_3_temp_col1 = limi_translation_4.x(Ao_3_col1)
        val Co_3_temp_col2 = limi_translation_4.x(Ao_3_col2)
        val Co_3_temp_col3 = limi_translation_4.x(Ao_3_col3)

        // Cos appended and Bo_t added makes final Co
        val Co_3_temp2 = Matrix(List(
            List(Co_3_temp_col1.at(0), Co_3_temp_col2.at(0), Co_3_temp_col3.at(0)),
            List(Co_3_temp_col1.at(1), Co_3_temp_col2.at(1), Co_3_temp_col3.at(1)),
            List(Co_3_temp_col1.at(2), Co_3_temp_col2.at(2), Co_3_temp_col3.at(2))
        ))
        val Co_3_temp2_t = Co_3_temp2.transpose()
        val Co_3 = Co_3_temp2 + Bo_3_temp_t

        val Bo_3 = Co_3.transpose()

        val Do_3_temp2 = Matrix(List(
            List(Do_3_temp2_row1.at(0), Do_3_temp2_row1.at(1), Do_3_temp2_row1.at(2)),
            List(Do_3_temp2_row2.at(0), Do_3_temp2_row2.at(1), Do_3_temp2_row2.at(2)),
            List(Do_3_temp2_row3.at(0), Do_3_temp2_row3.at(1), Do_3_temp2_row3.at(2))
        ))

        val Do_3_temp2_t = Do_3_temp2.transpose()
        val Do_3_temp2_col1: Vector = Do_3_temp2_t.row(0)
        val Do_3_temp2_col2: Vector = Do_3_temp2_t.row(1)
        val Do_3_temp2_col3: Vector = Do_3_temp2_t.row(2)

        val Bo_3_col1 = Co_3.row(0)
        val Bo_3_col2 = Co_3.row(1)
        val Bo_3_col3 = Co_3.row(2)

        val Do_3_temp3_col1 = limi_translation_4.x(Bo_3_col1)
        val Do_3_temp3_col2 = limi_translation_4.x(Bo_3_col2)
        val Do_3_temp3_col3 = limi_translation_4.x(Bo_3_col3)

        val Do_3_temp4_col1 = Do_3_temp2_col1 + Do_3_temp3_col1
        val Do_3_temp4_col2 = Do_3_temp2_col2 + Do_3_temp3_col2
        val Do_3_temp4_col3 = Do_3_temp2_col3 + Do_3_temp3_col3

        val Do_3 = Matrix(List(
            List(Do_3_temp4_col1.at(0), Do_3_temp4_col2.at(0), Do_3_temp4_col3.at(0)),
            List(Do_3_temp4_col1.at(1), Do_3_temp4_col2.at(1), Do_3_temp4_col3.at(1)),
            List(Do_3_temp4_col1.at(2), Do_3_temp4_col2.at(2), Do_3_temp4_col3.at(2))
        ))

        val Ia_3_add = Matrix(List(
            List(Ao_3.at(0, 0), Ao_3.at(0, 1), Ao_3.at(0, 2), Bo_3.at(0, 0), Bo_3.at(0, 1), Bo_3.at(0, 2)),
            List(Ao_3.at(1, 0), Ao_3.at(1, 1), Ao_3.at(1, 2), Bo_3.at(1, 0), Bo_3.at(1, 1), Bo_3.at(1, 2)),
            List(Ao_3.at(2, 0), Ao_3.at(2, 1), Ao_3.at(2, 2), Bo_3.at(2, 0), Bo_3.at(2, 1), Bo_3.at(2, 2)),
            List(Co_3.at(0, 0), Co_3.at(0, 1), Co_3.at(0, 2), Do_3.at(0, 0), Do_3.at(0, 1), Do_3.at(0, 2)),
            List(Co_3.at(1, 0), Co_3.at(1, 1), Co_3.at(1, 2), Do_3.at(1, 0), Do_3.at(1, 1), Do_3.at(1, 2)),
            List(Co_3.at(2, 0), Co_3.at(2, 1), Co_3.at(2, 2), Do_3.at(2, 0), Do_3.at(2, 1), Do_3.at(2, 2))
        ))

        val Ia_3_predec = Matrix(List(
            List(3.5879, 0.0, 0.0, -0.0, 0.0985021, -0.374644),
            List(0.0, 3.5879, 0.0, -0.0985021, -0.0, -0.190768),
            List(0.0, 0.0, 3.5879, 0.374644, 0.190768, -0.0),
            List(0.0, -0.0985021, 0.374644, 0.0676773, 0.0277158, 0.00390536),
            List(0.0985021, 0.0, 0.190768, 0.0277158, 0.0323994, -0.00164449),
            List(-0.374644, -0.190768, 0.0, 0.00390536, -0.00164449, 0.0775861)
        ))

        val Ia_3_temp = Ia_3_predec + Ia_3_add

        // JOINT 3
        val U_3 = Matrix(List(
            List(Ia_3_temp.at(5, 0)),
            List(Ia_3_temp.at(5, 1)),
            List(Ia_3_temp.at(5, 2)),
            List(Ia_3_temp.at(5, 3)),
            List(Ia_3_temp.at(5, 4)),
            List(Ia_3_temp.at(5, 5))
        ))
        val Dinv_3 = 1.0 / Ia_3_temp.at(5, 5)
        val UDinv_3 = U_3 * Dinv_3
        val U_3_t = U_3.transpose()
        val Ia_3_minus = UDinv_3.x(U_3_t)
        val Ia_3 = Ia_3_temp - Ia_3_minus

        // ActOn

        val Ai_2 = Ia_3.slice(0, 0)(3, 3)
        val Bi_2 = Ia_3.slice(0, 3)(3, 3)
        val Ci_2 = Ia_3.slice(3, 0)(3, 3)
        val Di_2 = Ia_3.slice(3, 3)(3, 3)

        val limi_rotation_3_t = limi_rotation_3.transpose()

        val Ao_2_temp1 = limi_rotation_3.x(Ai_2)
        val Ao_2 = Ao_2_temp1.x(limi_rotation_3_t)
        val Bo_2_temp1 = limi_rotation_3.x(Bi_2)
        val Bo_2_temp = Bo_2_temp1.x(limi_rotation_3_t)
        val Do_2_temp1 = limi_rotation_3.x(Di_2)
        val Do_2_temp = Do_2_temp1.x(limi_rotation_3_t)

        val Bo_2_temp_t = Bo_2_temp.transpose()
        val Bo_2_temp_col1: Vector = Bo_2_temp_t.row(0)
        val Bo_2_temp_col2: Vector = Bo_2_temp_t.row(1)
        val Bo_2_temp_col3: Vector = Bo_2_temp_t.row(2)

        val Do_2_temp_row1 = Do_2_temp.row(0)
        val Do_2_temp_row2 = Do_2_temp.row(1)
        val Do_2_temp_row3 = Do_2_temp.row(2)

        val Do_2_temp2_row1_temp = limi_translation_3.x(Bo_2_temp_col1)
        val Do_2_temp2_row1 = Do_2_temp_row1 + Do_2_temp2_row1_temp
        val Do_2_temp2_row2_temp = limi_translation_3.x(Bo_2_temp_col2)
        val Do_2_temp2_row2 = Do_2_temp_row2 + Do_2_temp2_row2_temp
        val Do_2_temp2_row3_temp = limi_translation_3.x(Bo_2_temp_col3)
        val Do_2_temp2_row3 = Do_2_temp_row3 + Do_2_temp2_row3_temp

        val Ao_2_t = Ao_2.transpose()
        val Ao_2_col1: Vector = Ao_2_t.row(0)
        val Ao_2_col2: Vector = Ao_2_t.row(1)
        val Ao_2_col3: Vector = Ao_2_t.row(2)

        val Co_2_temp_col1 = limi_translation_3.x(Ao_2_col1)
        val Co_2_temp_col2 = limi_translation_3.x(Ao_2_col2)
        val Co_2_temp_col3 = limi_translation_3.x(Ao_2_col3)

        // Cos appended and Bo_t added makes final Co
        val Co_2_temp2 = Matrix(List(
            List(Co_2_temp_col1.at(0), Co_2_temp_col2.at(0), Co_2_temp_col3.at(0)),
            List(Co_2_temp_col1.at(1), Co_2_temp_col2.at(1), Co_2_temp_col3.at(1)),
            List(Co_2_temp_col1.at(2), Co_2_temp_col2.at(2), Co_2_temp_col3.at(2))
        ))
        val Co_2_temp2_t = Co_2_temp2.transpose()
        val Co_2 = Co_2_temp2 + Bo_2_temp_t

        val Bo_2 = Co_2.transpose()

        val Do_2_temp2 = Matrix(List(
            List(Do_2_temp2_row1.at(0), Do_2_temp2_row1.at(1), Do_2_temp2_row1.at(2)),
            List(Do_2_temp2_row2.at(0), Do_2_temp2_row2.at(1), Do_2_temp2_row2.at(2)),
            List(Do_2_temp2_row3.at(0), Do_2_temp2_row3.at(1), Do_2_temp2_row3.at(2))
        ))

        val Do_2_temp2_t = Do_2_temp2.transpose()
        val Do_2_temp2_col1: Vector = Do_2_temp2_t.row(0)
        val Do_2_temp2_col2: Vector = Do_2_temp2_t.row(1)
        val Do_2_temp2_col3: Vector = Do_2_temp2_t.row(2)

        val Bo_2_col1 = Co_2.row(0)
        val Bo_2_col2 = Co_2.row(1)
        val Bo_2_col3 = Co_2.row(2)

        val Do_2_temp3_col1 = limi_translation_3.x(Bo_2_col1)
        val Do_2_temp3_col2 = limi_translation_3.x(Bo_2_col2)
        val Do_2_temp3_col3 = limi_translation_3.x(Bo_2_col3)

        val Do_2_temp4_col1 = Do_2_temp2_col1 + Do_2_temp3_col1
        val Do_2_temp4_col2 = Do_2_temp2_col2 + Do_2_temp3_col2
        val Do_2_temp4_col3 = Do_2_temp2_col3 + Do_2_temp3_col3

        val Do_2 = Matrix(List(
            List(Do_2_temp4_col1.at(0), Do_2_temp4_col2.at(0), Do_2_temp4_col3.at(0)),
            List(Do_2_temp4_col1.at(1), Do_2_temp4_col2.at(1), Do_2_temp4_col3.at(1)),
            List(Do_2_temp4_col1.at(2), Do_2_temp4_col2.at(2), Do_2_temp4_col3.at(2))
        ))

        val Ia_2_add = Matrix(List(
            List(Ao_2.at(0, 0), Ao_2.at(0, 1), Ao_2.at(0, 2), Bo_2.at(0, 0), Bo_2.at(0, 1), Bo_2.at(0, 2)),
            List(Ao_2.at(1, 0), Ao_2.at(1, 1), Ao_2.at(1, 2), Bo_2.at(1, 0), Bo_2.at(1, 1), Bo_2.at(1, 2)),
            List(Ao_2.at(2, 0), Ao_2.at(2, 1), Ao_2.at(2, 2), Bo_2.at(2, 0), Bo_2.at(2, 1), Bo_2.at(2, 2)),
            List(Co_2.at(0, 0), Co_2.at(0, 1), Co_2.at(0, 2), Do_2.at(0, 0), Do_2.at(0, 1), Do_2.at(0, 2)),
            List(Co_2.at(1, 0), Co_2.at(1, 1), Co_2.at(1, 2), Do_2.at(1, 0), Do_2.at(1, 1), Do_2.at(1, 2)),
            List(Co_2.at(2, 0), Co_2.at(2, 1), Co_2.at(2, 2), Do_2.at(2, 0), Do_2.at(2, 1), Do_2.at(2, 2))
        ))

        val Ia_2_predec = Matrix(List(
            List(3.2286, 0.0, 0.0, -0.0, -0.214709, -0.126729),
            List(0.0, 3.2286, 0.0, 0.214709, -0.0, 0.0888447),
            List(0.0, 0.0, 3.2286, 0.126729, -0.0888447, -0.0),
            List(0.0, 0.214709, 0.126729, 0.0564949, -0.00824833, -0.00548765),
            List(-0.214709, 0.0, -0.0888447, -0.00824833, 0.0528784, -0.00437726),
            List(-0.126729, 0.0888447, 0.0, -0.00548765, -0.00437726, 0.0182492)
        ))

        val Ia_2_temp = Ia_2_predec + Ia_2_add

        // JOINT 2
        val U_2 = Matrix(List(
            List(Ia_2_temp.at(5, 0)),
            List(Ia_2_temp.at(5, 1)),
            List(Ia_2_temp.at(5, 2)),
            List(Ia_2_temp.at(5, 3)),
            List(Ia_2_temp.at(5, 4)),
            List(Ia_2_temp.at(5, 5))
        ))
        val Dinv_2 = 1.0 / Ia_2_temp.at(5, 5)
        val UDinv_2 = U_2 * Dinv_2
        val U_2_t = U_2.transpose()
        val Ia_2_minus = UDinv_2.x(U_2_t)
        val Ia_2 = Ia_2_temp - Ia_2_minus

        // ActOn

        val Ai_1 = Ia_2.slice(0, 0)(3, 3)
        val Bi_1 = Ia_2.slice(0, 3)(3, 3)
        val Ci_1 = Ia_2.slice(3, 0)(3, 3)
        val Di_1 = Ia_2.slice(3, 3)(3, 3)

        val limi_rotation_2_t = limi_rotation_2.transpose()

        val Ao_1_temp1 = limi_rotation_2.x(Ai_1)
        val Ao_1 = Ao_1_temp1.x(limi_rotation_2_t)
        val Bo_1_temp1 = limi_rotation_2.x(Bi_1)
        val Bo_1_temp = Bo_1_temp1.x(limi_rotation_2_t)
        val Do_1_temp1 = limi_rotation_2.x(Di_1)
        val Do_1_temp = Do_1_temp1.x(limi_rotation_2_t)

        val Bo_1_temp_t = Bo_1_temp.transpose()
        val Bo_1_temp_col1: Vector = Bo_1_temp_t.row(0)
        val Bo_1_temp_col2: Vector = Bo_1_temp_t.row(1)
        val Bo_1_temp_col3: Vector = Bo_1_temp_t.row(2)

        val Do_1_temp_row1 = Do_1_temp.row(0)
        val Do_1_temp_row2 = Do_1_temp.row(1)
        val Do_1_temp_row3 = Do_1_temp.row(2)

        val Do_1_temp2_row1_temp = limi_translation_2.x(Bo_1_temp_col1)
        val Do_1_temp2_row1 = Do_1_temp_row1 + Do_1_temp2_row1_temp
        val Do_1_temp2_row2_temp = limi_translation_2.x(Bo_1_temp_col2)
        val Do_1_temp2_row2 = Do_1_temp_row2 + Do_1_temp2_row2_temp
        val Do_1_temp2_row3_temp = limi_translation_2.x(Bo_1_temp_col3)
        val Do_1_temp2_row3 = Do_1_temp_row3 + Do_1_temp2_row3_temp

        val Ao_1_t = Ao_1.transpose()
        val Ao_1_col1: Vector = Ao_1_t.row(0)
        val Ao_1_col2: Vector = Ao_1_t.row(1)
        val Ao_1_col3: Vector = Ao_1_t.row(2)

        val Co_1_temp_col1 = limi_translation_2.x(Ao_1_col1)
        val Co_1_temp_col2 = limi_translation_2.x(Ao_1_col2)
        val Co_1_temp_col3 = limi_translation_2.x(Ao_1_col3)

        // Cos appended and Bo_t added makes final Co
        val Co_1_temp2 = Matrix(List(
            List(Co_1_temp_col1.at(0), Co_1_temp_col2.at(0), Co_1_temp_col3.at(0)),
            List(Co_1_temp_col1.at(1), Co_1_temp_col2.at(1), Co_1_temp_col3.at(1)),
            List(Co_1_temp_col1.at(2), Co_1_temp_col2.at(2), Co_1_temp_col3.at(2))
        ))
        val Co_1_temp2_t = Co_1_temp2.transpose()
        val Co_1 = Co_1_temp2 + Bo_1_temp_t

        val Bo_1 = Co_1.transpose()

        val Do_1_temp2 = Matrix(List(
            List(Do_1_temp2_row1.at(0), Do_1_temp2_row1.at(1), Do_1_temp2_row1.at(2)),
            List(Do_1_temp2_row2.at(0), Do_1_temp2_row2.at(1), Do_1_temp2_row2.at(2)),
            List(Do_1_temp2_row3.at(0), Do_1_temp2_row3.at(1), Do_1_temp2_row3.at(2))
        ))

        val Do_1_temp2_t = Do_1_temp2.transpose()
        val Do_1_temp2_col1: Vector = Do_1_temp2_t.row(0)
        val Do_1_temp2_col2: Vector = Do_1_temp2_t.row(1)
        val Do_1_temp2_col3: Vector = Do_1_temp2_t.row(2)

        val Bo_1_col1 = Co_1.row(0)
        val Bo_1_col2 = Co_1.row(1)
        val Bo_1_col3 = Co_1.row(2)

        val Do_1_temp3_col1 = limi_translation_2.x(Bo_1_col1)
        val Do_1_temp3_col2 = limi_translation_2.x(Bo_1_col2)
        val Do_1_temp3_col3 = limi_translation_2.x(Bo_1_col3)

        val Do_1_temp4_col1 = Do_1_temp2_col1 + Do_1_temp3_col1
        val Do_1_temp4_col2 = Do_1_temp2_col2 + Do_1_temp3_col2
        val Do_1_temp4_col3 = Do_1_temp2_col3 + Do_1_temp3_col3

        val Do_1 = Matrix(List(
            List(Do_1_temp4_col1.at(0), Do_1_temp4_col2.at(0), Do_1_temp4_col3.at(0)),
            List(Do_1_temp4_col1.at(1), Do_1_temp4_col2.at(1), Do_1_temp4_col3.at(1)),
            List(Do_1_temp4_col1.at(2), Do_1_temp4_col2.at(2), Do_1_temp4_col3.at(2))
        ))

        val Ia_1_add = Matrix(List(
            List(Ao_1.at(0, 0), Ao_1.at(0, 1), Ao_1.at(0, 2), Bo_1.at(0, 0), Bo_1.at(0, 1), Bo_1.at(0, 2)),
            List(Ao_1.at(1, 0), Ao_1.at(1, 1), Ao_1.at(1, 2), Bo_1.at(1, 0), Bo_1.at(1, 1), Bo_1.at(1, 2)),
            List(Ao_1.at(2, 0), Ao_1.at(2, 1), Ao_1.at(2, 2), Bo_1.at(2, 0), Bo_1.at(2, 1), Bo_1.at(2, 2)),
            List(Co_1.at(0, 0), Co_1.at(0, 1), Co_1.at(0, 2), Do_1.at(0, 0), Do_1.at(0, 1), Do_1.at(0, 2)),
            List(Co_1.at(1, 0), Co_1.at(1, 1), Co_1.at(1, 2), Do_1.at(1, 0), Do_1.at(1, 1), Do_1.at(1, 2)),
            List(Co_1.at(2, 0), Co_1.at(2, 1), Co_1.at(2, 2), Do_1.at(2, 0), Do_1.at(2, 1), Do_1.at(2, 2))
        ))

        val Ia_1_predec = Matrix(List(
            List(0.646926, 0.0, 0.0, -0.0, 0.00226101, 0.0185797),
            List(0.0, 0.646926, 0.0, -0.00226101, -0.0, -0.00203199),
            List(0.0, 0.0, 0.646926, -0.0185797, 0.00203199, -0.0),
            List(0.0, -0.00226101, -0.0185797, 0.00850351, -0.00398336, 0.0102611),
            List(0.00226101, 0.0, 0.00203199, -0.00398336, 0.0281243, 0.000768936),
            List(0.0185797, -0.00203199, 0.0, 0.0102611, 0.000768936, 0.026535)
        ))

        val Ia_1_temp = Ia_1_predec + Ia_1_add

        // JOINT 1
        val U_1 = Matrix(List(
            List(Ia_1_temp.at(5, 0)),
            List(Ia_1_temp.at(5, 1)),
            List(Ia_1_temp.at(5, 2)),
            List(Ia_1_temp.at(5, 3)),
            List(Ia_1_temp.at(5, 4)),
            List(Ia_1_temp.at(5, 5))
        ))
        val Dinv_1 = 1.0 / Ia_1_temp.at(5, 5)
        val UDinv_1 = U_1 * Dinv_1
        val U_1_t = U_1.transpose()
        val Ia_1_minus = UDinv_1.x(U_1_t)
        val Ia_1 = Ia_1_temp - Ia_1_minus

        // ActOn

        val Ai_0 = Ia_1.slice(0, 0)(3, 3)
        val Bi_0 = Ia_1.slice(0, 3)(3, 3)
        val Ci_0 = Ia_1.slice(3, 0)(3, 3)
        val Di_0 = Ia_1.slice(3, 3)(3, 3)

        val limi_rotation_1_t = limi_rotation_1.transpose()

        val Ao_0_temp1 = limi_rotation_1.x(Ai_0)
        val Ao_0 = Ao_0_temp1.x(limi_rotation_1_t)
        val Bo_0_temp1 = limi_rotation_1.x(Bi_0)
        val Bo_0_temp = Bo_0_temp1.x(limi_rotation_1_t)
        val Do_0_temp1 = limi_rotation_1.x(Di_0)
        val Do_0_temp = Do_0_temp1.x(limi_rotation_1_t)

        val Bo_0_temp_t = Bo_0_temp.transpose()
        val Bo_0_temp_col1: Vector = Bo_0_temp_t.row(0)
        val Bo_0_temp_col2: Vector = Bo_0_temp_t.row(1)
        val Bo_0_temp_col3: Vector = Bo_0_temp_t.row(2)

        val Do_0_temp_row1 = Do_0_temp.row(0)
        val Do_0_temp_row2 = Do_0_temp.row(1)
        val Do_0_temp_row3 = Do_0_temp.row(2)

        val Do_0_temp2_row1_temp = limi_translation_1.x(Bo_0_temp_col1)
        val Do_0_temp2_row1 = Do_0_temp_row1 + Do_0_temp2_row1_temp
        val Do_0_temp2_row2_temp = limi_translation_1.x(Bo_0_temp_col2)
        val Do_0_temp2_row2 = Do_0_temp_row2 + Do_0_temp2_row2_temp
        val Do_0_temp2_row3_temp = limi_translation_1.x(Bo_0_temp_col3)
        val Do_0_temp2_row3 = Do_0_temp_row3 + Do_0_temp2_row3_temp

        val Ao_0_t = Ao_0.transpose()
        val Ao_0_col1: Vector = Ao_0_t.row(0)
        val Ao_0_col2: Vector = Ao_0_t.row(1)
        val Ao_0_col3: Vector = Ao_0_t.row(2)

        val Co_0_temp_col1 = limi_translation_1.x(Ao_0_col1)
        val Co_0_temp_col2 = limi_translation_1.x(Ao_0_col2)
        val Co_0_temp_col3 = limi_translation_1.x(Ao_0_col3)

        // Cos appended and Bo_t added makes final Co
        val Co_0_temp2 = Matrix(List(
            List(Co_0_temp_col1.at(0), Co_0_temp_col2.at(0), Co_0_temp_col3.at(0)),
            List(Co_0_temp_col1.at(1), Co_0_temp_col2.at(1), Co_0_temp_col3.at(1)),
            List(Co_0_temp_col1.at(2), Co_0_temp_col2.at(2), Co_0_temp_col3.at(2))
        ))
        val Co_0_temp2_t = Co_0_temp2.transpose()
        val Co_0 = Co_0_temp2 + Bo_0_temp_t

        val Bo_0 = Co_0.transpose()

        val Do_0_temp2 = Matrix(List(
            List(Do_0_temp2_row1.at(0), Do_0_temp2_row1.at(1), Do_0_temp2_row1.at(2)),
            List(Do_0_temp2_row2.at(0), Do_0_temp2_row2.at(1), Do_0_temp2_row2.at(2)),
            List(Do_0_temp2_row3.at(0), Do_0_temp2_row3.at(1), Do_0_temp2_row3.at(2))
        ))

        val Do_0_temp2_t = Do_0_temp2.transpose()
        val Do_0_temp2_col1: Vector = Do_0_temp2_t.row(0)
        val Do_0_temp2_col2: Vector = Do_0_temp2_t.row(1)
        val Do_0_temp2_col3: Vector = Do_0_temp2_t.row(2)

        val Bo_0_col1 = Co_0.row(0)
        val Bo_0_col2 = Co_0.row(1)
        val Bo_0_col3 = Co_0.row(2)

        val Do_0_temp3_col1 = limi_translation_1.x(Bo_0_col1)
        val Do_0_temp3_col2 = limi_translation_1.x(Bo_0_col2)
        val Do_0_temp3_col3 = limi_translation_1.x(Bo_0_col3)

        val Do_0_temp4_col1 = Do_0_temp2_col1 + Do_0_temp3_col1
        val Do_0_temp4_col2 = Do_0_temp2_col2 + Do_0_temp3_col2
        val Do_0_temp4_col3 = Do_0_temp2_col3 + Do_0_temp3_col3

        val Do_0 = Matrix(List(
            List(Do_0_temp4_col1.at(0), Do_0_temp4_col2.at(0), Do_0_temp4_col3.at(0)),
            List(Do_0_temp4_col1.at(1), Do_0_temp4_col2.at(1), Do_0_temp4_col3.at(1)),
            List(Do_0_temp4_col1.at(2), Do_0_temp4_col2.at(2), Do_0_temp4_col3.at(2))
        ))

        val Ia_0_add = Matrix(List(
            List(Ao_0.at(0, 0), Ao_0.at(0, 1), Ao_0.at(0, 2), Bo_0.at(0, 0), Bo_0.at(0, 1), Bo_0.at(0, 2)),
            List(Ao_0.at(1, 0), Ao_0.at(1, 1), Ao_0.at(1, 2), Bo_0.at(1, 0), Bo_0.at(1, 1), Bo_0.at(1, 2)),
            List(Ao_0.at(2, 0), Ao_0.at(2, 1), Ao_0.at(2, 2), Bo_0.at(2, 0), Bo_0.at(2, 1), Bo_0.at(2, 2)),
            List(Co_0.at(0, 0), Co_0.at(0, 1), Co_0.at(0, 2), Do_0.at(0, 0), Do_0.at(0, 1), Do_0.at(0, 2)),
            List(Co_0.at(1, 0), Co_0.at(1, 1), Co_0.at(1, 2), Do_0.at(1, 0), Do_0.at(1, 1), Do_0.at(1, 2)),
            List(Co_0.at(2, 0), Co_0.at(2, 1), Co_0.at(2, 2), Do_0.at(2, 0), Do_0.at(2, 1), Do_0.at(2, 2))
        ))

        //[4.97068, 0.0, 0.0, -0.0, -0.236704, -0.010344], 
        //    [0.0, 4.97068, 0.0, 0.236704, -0.0, 0.0192614], 
        //    [0.0, 0.0, 4.97068, 0.010344, -0.0192614, -0.0], 
        //    [0.0, 0.236704, 0.010344, 0.714663, -0.000179083, 0.00768923], 
        //    [-0.236704, 0.0, -0.0192614, -0.000179083, 0.717956, 0.0196616], 
        //    [-0.010344, 0.0192614, 0.0, 0.00768923, 0.0196616, 0.00921316]]
        val Ia_0_predec = Matrix(List(
            List(4.97068, 0.0, 0.0, -0.0, -0.236704, -0.010344),
            List(0.0, 4.97068, 0.0, 0.236704, -0.0, 0.0192614),
            List(0.0, 0.0, 4.97068, 0.010344, -0.0192614, -0.0),
            List(0.0, 0.236704, 0.010344, 0.714663, -0.000179083, 0.00768923),
            List(-0.236704, 0.0, -0.0192614, -0.000179083, 0.717956, 0.0196616),
            List(-0.010344, 0.0192614, 0.0, 0.00768923, 0.0196616, 0.00921316)
        ))

        val Ia_0_temp = Ia_0_predec + Ia_0_add

        // JOINT 0
        val U_0 = Matrix(List(
            List(Ia_0_temp.at(5, 0)),
            List(Ia_0_temp.at(5, 1)),
            List(Ia_0_temp.at(5, 2)),
            List(Ia_0_temp.at(5, 3)),
            List(Ia_0_temp.at(5, 4)),
            List(Ia_0_temp.at(5, 5))
        ))
        val Dinv_0 = 1.0 / Ia_0_temp.at(5, 5)
        val UDinv_0 = U_0 * Dinv_0
        val U_0_t = U_0.transpose()
        val Ia_0_minus = UDinv_0.x(U_0_t)
        val Ia_0 = Ia_0_temp - Ia_0_minus



        Dinv_5
    } ensuring(res => res +/- 1e-5)

}

