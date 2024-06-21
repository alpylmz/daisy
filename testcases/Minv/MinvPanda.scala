import daisy.lang._
import Real._
import javax.security.sasl.RealmCallback




object Minv {
    def firstPass(
        qpos1: Real,
        qpos2: Real,
        qpos3: Real,
        qpos4: Real,
        qpos5: Real
    ) = {
        require(
            qpos1 > -3.2 && qpos1 < 3.2 &&
            qpos2 > -3.2 && qpos2 < 3.2 &&
            qpos3 > -3.2 && qpos3 < 3.2 &&
            qpos4 > -3.2 && qpos4 < 3.2 &&
            qpos5 > -3.2 && qpos5 < 3.2
        )
        val sin_qpos1 = sin(qpos1)
        val cos_qpos1 = cos(qpos1)
        val sin_qpos2 = sin(qpos2)
        val cos_qpos2 = cos(qpos2)
        val sin_qpos3 = sin(qpos3)
        val cos_qpos3 = cos(qpos3)
        val sin_qpos4 = sin(qpos4)
        val cos_qpos4 = cos(qpos4)
        val sin_qpos5 = sin(qpos5)
        val cos_qpos5 = cos(qpos5)

        // JOINT 1

        val rotation_matrix_1_1_1 = cos_qpos1
        val rotation_matrix_1_1_2 = -sin_qpos1
        val rotation_matrix_1_2_1 = sin_qpos1
        val rotation_matrix_1_2_2 = cos_qpos1
        // val rotation_matrix_1_3_3 = 1.0 for some reason daisy does not like this line

        val limi_rotation_1_1_1 = rotation_matrix_1_1_1
        val limi_rotation_1_1_2 = rotation_matrix_1_1_2
        //val limi_rotation_1_1_3 = 0.0
        val limi_rotation_1_2_1 = rotation_matrix_1_2_1
        val limi_rotation_1_2_2 = rotation_matrix_1_2_2
        //val limi_rotation_1_2_3 = 0.0
        //val limi_rotation_1_3_1 = 0.0
        //val limi_rotation_1_3_2 = 0.0
        //val limi_rotation_1_3_3 = 1.0

        //val limi_translation_1_1 = 0.0
        //val limi_translation_1_2 = 0.0
        //val limi_translation_1_3 = 0.333

        // parent = 0, so oMi is limi
        val oMi_rotation_1_1_1 = limi_rotation_1_1_1
        val oMi_rotation_1_1_2 = limi_rotation_1_1_2
        //val oMi_rotation_1_1_3 = limi_rotation_1_1_3
        val oMi_rotation_1_2_1 = limi_rotation_1_2_1
        val oMi_rotation_1_2_2 = limi_rotation_1_2_2
        //val oMi_rotation_1_2_3 = limi_rotation_1_2_3
        //val oMi_rotation_1_3_1 = limi_rotation_1_3_1
        //val oMi_rotation_1_3_2 = limi_rotation_1_3_2
        //val oMi_rotation_1_3_3 = limi_rotation_1_3_3

        //val oMi_translation_1_1 = limi_translation_1_1
        //val oMi_translation_1_2 = limi_translation_1_2
        //val oMi_translation_1_3 = limi_translation_1_3

        // JOINT 2

        val rotation_matrix_2_1_1 = cos_qpos2
        val rotation_matrix_2_1_2 = -sin_qpos2
        val rotation_matrix_2_2_1 = sin_qpos2
        val rotation_matrix_2_2_2 = cos_qpos2
        // val rotation_matrix_2_3_3 = 1.0

        val limi_rotation_2_1_1 = rotation_matrix_2_1_1
        val limi_rotation_2_1_2 = rotation_matrix_2_1_2
        //val limi_rotation_2_1_3 = 0.0
        //val limi_rotation_2_2_1 = 0.0
        //val limi_rotation_2_2_2 = 0.0
        //val limi_rotation_2_2_3 = 1.0
        val limi_rotation_2_3_1 = -rotation_matrix_2_2_1
        val limi_rotation_2_3_2 = -rotation_matrix_2_2_2
        //val limi_rotation_2_3_3 = 0.0

        // These 3 are always 0 for some reason
        //val limi_translation_2_1 = model_joint_p_translation_2_1
        //val limi_translation_2_2 = model_joint_p_translation_2_2
        //val limi_translation_2_3 = model_joint_p_translation_2_3

        // parent is 1, so oMi is oMi[1] * limi
        val oMi_rotation_2_1_1 = oMi_rotation_1_1_1 * limi_rotation_2_1_1
        val oMi_rotation_2_1_2 = oMi_rotation_1_1_1 * limi_rotation_2_1_2
        val oMi_rotation_2_1_3 = oMi_rotation_1_1_2
        val oMi_rotation_2_2_1 = oMi_rotation_1_2_1 * limi_rotation_2_1_1
        val oMi_rotation_2_2_2 = oMi_rotation_1_2_1 * limi_rotation_2_1_2
        val oMi_rotation_2_2_3 = oMi_rotation_1_2_2
        val oMi_rotation_2_3_1 = limi_rotation_2_3_1
        val oMi_rotation_2_3_2 = limi_rotation_2_3_2
        //val oMi_rotation_2_3_3 = 0.0

        //val oMi_translation_2_1 = oMi_translation_1_1 // 0.0
        //val oMi_translation_2_2 = oMi_translation_1_2 // 0.0
        //val oMi_translation_2_3 = oMi_translation_1_3 // 0.333

        // JOINT 3

        val rotation_matrix_3_1_1 = cos_qpos3
        val rotation_matrix_3_1_2 = -sin_qpos3
        val rotation_matrix_3_2_1 = sin_qpos3
        val rotation_matrix_3_2_2 = cos_qpos3
        // val rotation_matrix_3_3_3 = 1.0

        val limi_rotation_3_1_1 = rotation_matrix_3_1_1
        val limi_rotation_3_1_2 = rotation_matrix_3_1_2
        //val limi_rotation_3_1_3 = 0.0
        //val limi_rotation_3_2_1 = 0.0
        //val limi_rotation_3_2_2 = 0.0
        //val limi_rotation_3_2_3 = -1
        val limi_rotation_3_3_1 = rotation_matrix_3_2_1
        val limi_rotation_3_3_2 = rotation_matrix_3_2_2
        //val limi_rotation_3_3_3 = 0.0

        //val limi_translation_3_1 = 0.0
        //val limi_translation_3_2 = -0.316
        //val limi_translation_3_3 = 0.0

        // parent is 2, so oMi is oMi[2] * limi
        val oMi_rotation_3_1_1 = oMi_rotation_2_1_1 * limi_rotation_3_1_1 + oMi_rotation_2_1_3 * limi_rotation_3_3_1
        val oMi_rotation_3_1_2 = oMi_rotation_2_1_1 * limi_rotation_3_1_2 + oMi_rotation_2_1_3 * limi_rotation_3_3_2
        val oMi_rotation_3_1_3 = -oMi_rotation_2_1_2
        val oMi_rotation_3_2_1 = oMi_rotation_2_2_1 * limi_rotation_3_1_1 + oMi_rotation_2_2_3 * limi_rotation_3_3_1
        val oMi_rotation_3_2_2 = oMi_rotation_2_2_1 * limi_rotation_3_1_2 + oMi_rotation_2_2_3 * limi_rotation_3_3_2
        val oMi_rotation_3_2_3 = -oMi_rotation_2_2_2
        val oMi_rotation_3_3_1 = oMi_rotation_2_3_1 * limi_rotation_3_1_1
        val oMi_rotation_3_3_2 = oMi_rotation_2_3_1 * limi_rotation_3_1_2
        val oMi_rotation_3_3_3 = -oMi_rotation_2_3_2

        val oMi_translation_3_1 = oMi_rotation_2_1_2 * -0.316
        val oMi_translation_3_2 = oMi_rotation_2_2_2 * -0.316
        val oMi_translation_3_3 = oMi_rotation_2_3_2 * -0.316 + 0.333

        // JOINT 4
        val rotation_matrix_4_1_1 = cos_qpos4
        val rotation_matrix_4_1_2 = -sin_qpos4
        val rotation_matrix_4_2_1 = sin_qpos4
        val rotation_matrix_4_2_2 = cos_qpos4
        // val rotation_matrix_4_3_3 = 1.0 for some reason daisy does not like this line
        
        val limi_rotation_4_1_1 = rotation_matrix_4_1_1
        val limi_rotation_4_1_2 = rotation_matrix_4_1_2
        //val limi_rotation_4_1_3 = 0.0
        //val limi_rotation_4_2_1 = 0.0
        //val limi_rotation_4_2_2 = 0.0
        //val limi_rotation_4_2_3 = -1
        val limi_rotation_4_3_1 = rotation_matrix_4_2_1
        val limi_rotation_4_3_2 = rotation_matrix_4_2_2
        //val limi_rotation_4_3_3 = 0.0

        //val limi_translation_4_1 = 0.083
        //val limi_translation_4_2 = 0.0
        //val limi_translation_4_3 = 0.0

        // parent is 3, so oMi is oMi[3] * limi
        val oMi_rotation_4_1_1 = oMi_rotation_3_1_1 * limi_rotation_4_1_1 + oMi_rotation_3_1_3 * limi_rotation_4_3_1
        val oMi_rotation_4_1_2 = oMi_rotation_3_1_1 * limi_rotation_4_1_2 + oMi_rotation_3_1_3 * limi_rotation_4_3_2
        val oMi_rotation_4_1_3 = -oMi_rotation_3_1_2
        val oMi_rotation_4_2_1 = oMi_rotation_3_2_1 * limi_rotation_4_1_1 + oMi_rotation_3_2_3 * limi_rotation_4_3_1
        val oMi_rotation_4_2_2 = oMi_rotation_3_2_1 * limi_rotation_4_1_2 + oMi_rotation_3_2_3 * limi_rotation_4_3_2
        val oMi_rotation_4_2_3 = -oMi_rotation_3_2_2
        val oMi_rotation_4_3_1 = oMi_rotation_3_3_1 * limi_rotation_4_1_1 + oMi_rotation_3_3_3 * limi_rotation_4_3_1
        val oMi_rotation_4_3_2 = oMi_rotation_3_3_1 * limi_rotation_4_1_2 + oMi_rotation_3_3_3 * limi_rotation_4_3_2
        val oMi_rotation_4_3_3 = -oMi_rotation_3_3_2

        val oMi_translation_4_1 = oMi_rotation_3_1_1 * 0.083 + oMi_translation_3_1
        val oMi_translation_4_2 = oMi_rotation_3_2_1 * 0.083 + oMi_translation_3_2
        val oMi_translation_4_3 = oMi_rotation_3_3_1 * 0.083 + oMi_translation_3_3

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
        val oMi_rotation_5_1_1 = oMi_rotation_4_1_1 * limi_rotation_5_1_1 + oMi_rotation_4_1_3 * limi_rotation_5_3_1
        val oMi_rotation_5_1_2 = oMi_rotation_4_1_1 * limi_rotation_5_1_2 + oMi_rotation_4_1_3 * limi_rotation_5_3_2
        val oMi_rotation_5_1_3 = oMi_rotation_4_1_2
        val oMi_rotation_5_2_1 = oMi_rotation_4_2_1 * limi_rotation_5_1_1 + oMi_rotation_4_2_3 * limi_rotation_5_3_1
        val oMi_rotation_5_2_2 = oMi_rotation_4_2_1 * limi_rotation_5_1_2 + oMi_rotation_4_2_3 * limi_rotation_5_3_2
        val oMi_rotation_5_2_3 = oMi_rotation_4_2_2
        val oMi_rotation_5_3_1 = oMi_rotation_4_3_1 * limi_rotation_5_1_1 + oMi_rotation_4_3_3 * limi_rotation_5_3_1
        val oMi_rotation_5_3_2 = oMi_rotation_4_3_1 * limi_rotation_5_1_2 + oMi_rotation_4_3_3 * limi_rotation_5_3_2
        val oMi_rotation_5_3_3 = oMi_rotation_4_3_2

        val oMi_translation_5_1 = oMi_rotation_4_1_1 * -0.083 + oMi_rotation_4_1_2 * 0.384 + oMi_translation_4_1
        val oMi_translation_5_2 = oMi_rotation_4_2_1 * -0.083 + oMi_rotation_4_2_2 * 0.384 + oMi_translation_4_2
        val oMi_translation_5_3 = oMi_rotation_4_3_1 * -0.083 + oMi_rotation_4_3_2 * 0.384 + oMi_translation_4_3

        oMi_rotation_5_1_1

    } ensuring(res => res +/- 1e-10)



  

}


