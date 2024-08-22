import daisy.lang._
import Real._
import javax.security.sasl.RealmCallback




object Minv {
    def firstPass(
        qpos5: Real,
        qpos6: Real
    ) = {
        require(
            qpos5 > 0.2 && qpos5 < 0.3 &&
            qpos6 > 0.2 && qpos6 < 0.3 
        )
        val sin_qpos5 = sin(qpos5)
        val cos_qpos5 = cos(qpos5)
        val sin_qpos6 = sin(qpos6)
        val cos_qpos6 = cos(qpos6)

        // JOINT 5
        val rotation_matrix_5_1_1 = cos_qpos5
        val rotation_matrix_5_1_2 = -sin_qpos5
        val rotation_matrix_5_2_1 = sin_qpos5
        val rotation_matrix_5_2_2 = cos_qpos5
        // val rotation_matrix_5_3_3 = 1.0 for some reason daisy does not like this line
        
        val limi_rotation_5_1_1 = rotation_matrix_5_1_1
        val limi_rotation_5_1_2 = rotation_matrix_5_1_2
        //val limi_rotation_5_1_3 = 0.0
        //val limi_rotation_5_2_1 = 0.0
        //val limi_rotation_5_2_2 = 0.0
        //val limi_rotation_5_2_3 = 1.0
        val limi_rotation_5_3_1 = -rotation_matrix_5_2_1
        val limi_rotation_5_3_2 = -rotation_matrix_5_2_2
        //val limi_rotation_5_3_3 = 0.0

        //val limi_translation_5_1 = -0.083
        //val limi_translation_5_2 = 0.384
        //val limi_translation_5_3 = 0.0

        // parent is 4, so oMi is oMi[4] * limi
        val oMi_rotation_5_1_1 = limi_rotation_5_1_1
        val oMi_rotation_5_1_2 = limi_rotation_5_1_2
        //val oMi_rotation_5_1_3 = limi_rotation_5_1_3 0.0
        //val oMi_rotation_5_2_1 = limi_rotation_5_2_1 0.0
        //val oMi_rotation_5_2_2 = limi_rotation_5_2_2 0.0
        //val oMi_rotation_5_2_3 = limi_rotation_5_2_3 1.0
        val oMi_rotation_5_3_1 = limi_rotation_5_3_1
        val oMi_rotation_5_3_2 = limi_rotation_5_3_2
        //val oMi_rotation_5_3_3 = limi_rotation_5_3_3 0.0

        //val oMi_translation_5_1 = 0.0
        //val oMi_translation_5_2 = 0.0
        //val oMi_translation_5_3 = 0.0

        // JOINT 6
        val rotation_matrix_6_1_1 = cos_qpos6
        val rotation_matrix_6_1_2 = -sin_qpos6
        val rotation_matrix_6_2_1 = sin_qpos6
        val rotation_matrix_6_2_2 = cos_qpos6
        // val rotation_matrix_4_3_3 = 1.0 for some reason daisy does not like this line
        
        val limi_rotation_6_1_1 = rotation_matrix_6_1_1
        val limi_rotation_6_1_2 = rotation_matrix_6_1_2
        //val limi_rotation_6_1_3 = 0.0
        //val limi_rotation_6_2_1 = 0.0
        //val limi_rotation_6_2_2 = 0.0
        //val limi_rotation_6_2_3 = -1.0
        val limi_rotation_6_3_1 = rotation_matrix_6_2_1
        val limi_rotation_6_3_2 = rotation_matrix_6_2_2
        //val limi_rotation_6_3_3 = 0.0

        //val limi_translation_6_1 = 0.0
        //val limi_translation_6_2 = 0.0
        //val limi_translation_6_3 = 0.0

        // parent is 3, so oMi is oMi[3] * limi
        val oMi_rotation_6_1_1 = oMi_rotation_5_1_1 * limi_rotation_6_1_1
        val oMi_rotation_6_1_2 = oMi_rotation_5_1_1 * limi_rotation_6_1_2
        val oMi_rotation_6_1_3 = -oMi_rotation_5_1_2
        val oMi_rotation_6_2_1 = limi_rotation_6_3_1
        val oMi_rotation_6_2_2 = limi_rotation_6_3_2
        //val oMi_rotation_6_2_3 = 0.0
        val oMi_rotation_6_3_1 = oMi_rotation_5_3_1 * limi_rotation_6_1_1
        val oMi_rotation_6_3_2 = oMi_rotation_5_3_1 * limi_rotation_6_1_2
        val oMi_rotation_6_3_3 = -oMi_rotation_5_3_2

        //val oMi_translation_6_1 = 0.0
        //val oMi_translation_6_2 = 0.0
        //val oMi_translation_6_3 = 0.0

        val temp1_6_1_1 = limi_rotation_6_1_1 * 2.253 + limi_rotation_6_1_2 * -0.374
        val temp1_6_1_2 = limi_rotation_6_1_1 * -0.374 + limi_rotation_6_1_2 * 1.463
        //val temp1_6_1_3 = limi_rotation_6_1_1 * 0.0 + limi_rotation_6_1_2 * 0.0
        //val temp1_6_2_1 = 0.0
        //val temp1_6_2_2 = 0.0
        //val temp1_6_2_3 = -2.402
        val temp1_6_3_1 = limi_rotation_6_3_1 * 2.253 + limi_rotation_6_3_2 * -0.374
        val temp1_6_3_2 = limi_rotation_6_3_1 * -0.374 + limi_rotation_6_3_2 * 1.463
        //val temp1_6_3_3 = limi_rotation_6_3_1 * 0.0 + limi_rotation_6_3_2 * 0.0

        // now Ao = temp1 * R.transpose()
        val limi_rotation_transpose_6_1_1 = limi_rotation_6_1_1
        //val limi_rotation_transpose_6_1_2 = 0.0
        val limi_rotation_transpose_6_1_3 = limi_rotation_6_3_1
        val limi_rotation_transpose_6_2_1 = limi_rotation_6_1_2
        //val limi_rotation_transpose_6_2_2 = 0.0
        val limi_rotation_transpose_6_2_3 = limi_rotation_6_3_2
        //val limi_rotation_transpose_6_3_1 = 0.0
        //val limi_rotation_transpose_6_3_2 = -1.0
        //val limi_rotation_transpose_6_3_3 = 0.0
        

        val Ao_6_1_1 = temp1_6_1_1 * limi_rotation_transpose_6_1_1 + temp1_6_1_2 * limi_rotation_transpose_6_2_1
        //val Ao_6_1_2 = 0.0  
        val Ao_6_1_3 = temp1_6_1_1 * limi_rotation_transpose_6_1_3 + temp1_6_1_2 * limi_rotation_transpose_6_2_3
        //val Ao_6_2_1 = 0.0
        //val Ao_6_2_2 = 2.402 
        //val Ao_6_2_3 = 0.0
        val Ao_6_3_1 = temp1_6_3_1 * limi_rotation_transpose_6_1_1 + temp1_6_3_2 * limi_rotation_transpose_6_2_1
        //val Ao_6_3_2 = 0.0
        val Ao_6_3_3 = temp1_6_3_1 * limi_rotation_transpose_6_1_3 + temp1_6_3_2 * limi_rotation_transpose_6_2_3

        val temp2_6_1_1 = limi_rotation_6_1_2 * 0.022
        val temp2_6_1_2 = limi_rotation_6_1_1 * -0.022 + limi_rotation_6_1_2 * -0.003
        //val temp2_6_1_3 = 0.0
        //val temp2_6_2_1 = 0.069
        //val temp2_6_2_2 = 0.173
        //val temp2_6_2_3 = 0.0
        val temp2_6_3_1 = limi_rotation_6_3_2 * 0.022
        val temp2_6_3_2 = limi_rotation_6_3_1 * -0.022 + limi_rotation_6_3_2 * -0.003
        //val temp2_6_3_3 = 0.0

        val Bo_temp_6_1_1 = temp2_6_1_1 * limi_rotation_transpose_6_1_1 + temp2_6_1_2 * limi_rotation_transpose_6_2_1
        //val Bo_6_1_2 = 0.0
        val Bo_temp_6_1_3 = temp2_6_1_1 * limi_rotation_transpose_6_1_3 + temp2_6_1_2 * limi_rotation_transpose_6_2_3
        val Bo_temp_6_2_1 = 0.069 * limi_rotation_transpose_6_1_1 + 0.173 * limi_rotation_transpose_6_2_1
        //val Bo_6_2_2 = 0.0
        val Bo_temp_6_2_3 = 0.069 * limi_rotation_transpose_6_1_3 + 0.173 * limi_rotation_transpose_6_2_3
        val Bo_temp_6_3_1 = temp2_6_3_1 * limi_rotation_transpose_6_1_1 + temp2_6_3_2 * limi_rotation_transpose_6_2_1
        //val Bo_temp_6_3_2 = 0.0
        val Bo_temp_6_3_3 = temp2_6_3_1 * limi_rotation_transpose_6_1_3 + temp2_6_3_2 * limi_rotation_transpose_6_2_3

        val temp3_6_1_1 = limi_rotation_6_1_1 * 0.018 + limi_rotation_6_1_2 * 0.007
        val temp3_6_1_2 = limi_rotation_6_1_1 * 0.007 + limi_rotation_6_1_2 * 0.023
        //val temp3_6_1_3 = 0.0
        //val temp3_6_2_1 = 0.0
        //val temp3_6_2_2 = 0.0
        //val temp3_6_2_3 = 0.0
        val temp3_6_3_1 = limi_rotation_6_3_1 * 0.018 + limi_rotation_6_3_2 * 0.007
        val temp3_6_3_2 = limi_rotation_6_3_1 * 0.007 + limi_rotation_6_3_2 * 0.023
        //val temp3_6_3_3 = 0.0

        val Do_temp_6_1_1 = temp3_6_1_1 * limi_rotation_transpose_6_1_1 + temp3_6_1_2 * limi_rotation_transpose_6_2_1
        //val Do_temp_6_1_2 = 0.0
        val Do_temp_6_1_3 = temp3_6_1_1 * limi_rotation_transpose_6_1_3 + temp3_6_1_2 * limi_rotation_transpose_6_2_3
        //val Do_temp_6_2_1 = 0.0
        //val Do_temp_6_2_2 = 0.0
        //val Do_temp_6_2_3 = 0.0
        val Do_temp_6_3_1 = temp3_6_3_1 * limi_rotation_transpose_6_1_1 + temp3_6_3_2 * limi_rotation_transpose_6_2_1
        //val Do_temp_6_3_2 = 0.0
        val Do_temp_6_3_3 = temp3_6_3_1 * limi_rotation_transpose_6_1_3 + temp3_6_3_2 * limi_rotation_transpose_6_2_3

        val Do_temp2_6_1_1 = Do_temp_6_1_1
        //val Do_temp2_6_1_2 = Do_temp_6_1_2 //0.0
        val Do_temp2_6_1_3 = Do_temp_6_1_3
        //val Do_temp2_6_2_1 = Do_temp_6_2_1 //0.0
        //val Do_temp2_6_2_2 = Do_temp_6_2_2 //0.0
        //val Do_temp2_6_2_3 = Do_temp_6_2_3 //0.0
        val Do_temp2_6_3_1 = Do_temp_6_3_1
        //val Do_temp2_6_3_2 = Do_temp_6_3_2 //0.0
        val Do_temp2_6_3_3 = Do_temp_6_3_3

        val Co_6_1_1 = Bo_temp_6_1_1
        //val Co_6_1_2 = Bo_temp_6_1_2 // 0.0
        val Co_6_1_3 = Bo_temp_6_1_3
        val Co_6_2_1 = Bo_temp_6_2_1
        //val Co_6_2_2 = Bo_temp_6_2_2 // 0.0
        val Co_6_2_3 = Bo_temp_6_2_3
        val Co_6_3_1 = Bo_temp_6_3_1
        //val Co_6_3_2 = Bo_temp_6_3_2 // 0.0
        val Co_6_3_3 = Bo_temp_6_3_3

        val Bo_6_1_1 = Co_6_1_1
        val Bo_6_1_2 = Co_6_2_1
        val Bo_6_1_3 = Co_6_3_1
        //val Bo_6_2_1 = Co_6_1_2 // 0.0
        //val Bo_6_2_2 = Co_6_2_2 // 0.0
        //val Bo_6_2_3 = Co_6_3_2 // 0.0
        val Bo_6_3_1 = Co_6_1_3
        val Bo_6_3_2 = Co_6_2_3
        val Bo_6_3_3 = Co_6_3_3

        val Do_6_1_1 = Do_temp2_6_1_1
        //val Do_6_1_2 = Do_temp2_6_1_2 // 0.0
        val Do_6_1_3 = Do_temp2_6_1_3
        //val Do_6_2_1 = Do_temp2_6_2_1 // 0.0
        //val Do_6_2_2 = Do_temp2_6_2_2 // 0.0
        //val Do_6_2_3 = Do_temp2_6_2_3 // 0.0
        val Do_6_3_1 = Do_temp2_6_3_1
        //val Do_6_3_2 = Do_temp2_6_3_2 // 0.0
        val Do_6_3_3 = Do_temp2_6_3_3

        val Ia_5_1_1 = Ao_6_1_1 + 1.22595
        //val Ia_5_1_2 = 0.0
        val Ia_5_1_3 = Ao_6_1_3 + 0.0
        //val Ia_5_2_1 = 0.0
        //val Ia_5_2_2 = 3.62795
        //val Ia_5_2_3 = 0.0
        val Ia_5_3_1 = Ao_6_3_1 + 0.0
        //val Ia_5_3_2 = 0.0
        val Ia_5_3_3 = Ao_6_3_3 + 1.22595
        val Ia_5_4_1 = Bo_6_1_1 + 0.0
        val Ia_5_4_2 = Bo_6_1_2 - 0.0471217
        val Ia_5_4_3 = Bo_6_1_3 - 0.0503435
        //val Ia_5_5_1 = 0.0471217
        //val Ia_5_5_2 = 0.0
        //val Ia_5_5_3 = 0.0146537
        val Ia_5_6_1 = Bo_6_3_1 + 0.0503435
        val Ia_5_6_2 = Bo_6_3_2 + 0.0146537
        val Ia_5_6_3 = Bo_6_3_3 + 0.0
        val Ia_5_1_4 = Co_6_1_1 + 0.0
        //val Ia_5_1_5 = -0.0471217
        val Ia_5_1_6 = Co_6_1_3 - 0.0503435
        val Ia_5_2_4 = Co_6_2_1 + 0.0471217
        //val Ia_5_2_5 = 0.0
        val Ia_5_2_6 = Co_6_2_3 - 0.0146537
        val Ia_5_3_4 = Co_6_3_1 + 0.0503435
        //val Ia_5_3_5 = 0.0146537
        val Ia_5_3_6 = Co_6_3_3 + 0.0
        val Ia_5_4_4 = Do_6_1_1 + 0.0394276
        //val Ia_5_4_5 = -0.00151524
        val Ia_5_4_6 = Do_6_1_3 - 0.00460025
        //val Ia_5_5_4 = 0.00151524
        //val Ia_5_5_5 = 0.0314604
        //val Ia_5_5_6 = -0.00216405
        val Ia_5_6_4 = Do_6_3_1 + 0.00460025
        //val Ia_5_6_5 = +0.00216405
        val Ia_5_6_6 = Do_6_3_3 + 0.0108695

        // JOINT 5
        val U_5_1 = Ia_5_1_6
        val U_5_2 = Ia_5_2_6
        val U_5_3 = Ia_5_3_6
        val U_5_4 = Ia_5_4_6
        //val U_5_5 = Ia_5_6_5 // 0.00216405
        val U_5_6 = Ia_5_6_6

        val Dinv_5_1 = 1.0 / U_5_6

        Dinv_5_1
        
    } ensuring(res => res +/- 1e-2)



  

}


