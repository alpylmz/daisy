import daisy.lang._
import Real._
import javax.security.sasl.RealmCallback




object Minv {
    def firstPass(
        qpos1: Real,
        qpos2: Real,
        qpos3: Real
    ) = {
        require(
            qpos1 > -3.2 && qpos1 < 3.2 &&
            qpos2 > -3.2 && qpos2 < 3.2 &&
            qpos3 > -3.2 && qpos3 < 3.2
        )
        val sin_qpos1 = sin(qpos1)
        val cos_qpos1 = cos(qpos1)
        val sin_qpos2 = sin(qpos2)
        val cos_qpos2 = cos(qpos2)
        val sin_qpos3 = sin(qpos3)
        val cos_qpos3 = cos(qpos3)

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

        oMi_rotation_3_1_1
        

    } ensuring(res => res +/- 1e-10)



  

}


