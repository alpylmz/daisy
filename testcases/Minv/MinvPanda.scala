import daisy.lang._
import Real._
import javax.security.sasl.RealmCallback




object Minv {
    def firstPass(
        qpos1: Real,
        qpos2: Real,
        qpos3: Real,
        qpos4: Real,
        qpos5: Real,
        qpos6: Real
    ) = {
        require(
            qpos1 > 0.1 && qpos1 < 2.8973 &&
            qpos2 > 0.1 && qpos2 < 1.7628 &&
            qpos3 > 0.1 && qpos3 < 2.8973 &&
            qpos4 > 0.1 && qpos4 < 2.8973 && // this is actually wrong
            //qpos5 > 0.1 && qpos5 < 2.8973 &&
            qpos5 > 0.2 && qpos5 < 0.6 &&
            //qpos6 > 0.1 && qpos6 < 3.1 // this is wrong, too
            qpos6 > 0.1 && qpos6 < 1.1 // this is good for zero by division exception
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
        val sin_qpos6 = sin(qpos6)
        val cos_qpos6 = cos(qpos6)

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
        val oMi_rotation_6_1_1 = oMi_rotation_5_1_1 * limi_rotation_6_1_1 + oMi_rotation_5_1_3 * limi_rotation_6_3_1
        val oMi_rotation_6_1_2 = oMi_rotation_5_1_1 * limi_rotation_6_1_2 + oMi_rotation_5_1_3 * limi_rotation_6_3_2
        val oMi_rotation_6_1_3 = -oMi_rotation_5_1_2
        val oMi_rotation_6_2_1 = oMi_rotation_5_2_1 * limi_rotation_6_1_1 + oMi_rotation_5_2_3 * limi_rotation_6_3_1
        val oMi_rotation_6_2_2 = oMi_rotation_5_2_1 * limi_rotation_6_1_2 + oMi_rotation_5_2_3 * limi_rotation_6_3_2
        val oMi_rotation_6_2_3 = -oMi_rotation_5_2_2
        val oMi_rotation_6_3_1 = oMi_rotation_5_3_1 * limi_rotation_6_1_1 + oMi_rotation_5_3_3 * limi_rotation_6_3_1
        val oMi_rotation_6_3_2 = oMi_rotation_5_3_1 * limi_rotation_6_1_2 + oMi_rotation_5_3_3 * limi_rotation_6_3_2
        val oMi_rotation_6_3_3 = -oMi_rotation_5_3_2

        val oMi_translation_6_1 = oMi_translation_5_1
        val oMi_translation_6_2 = oMi_translation_5_2
        val oMi_translation_6_3 = oMi_translation_5_3

        //oMi_rotation_6_1_1
        
        // Inertia matrices for each joint are constant
        // JOINT 1:
        //[[4.97068, 0.0, 0.0, -0.0, -0.236704, -0.010344], [0.0, 4.97068, 0.0, 0.236704, -0.0, 0.0192614], [0.0, 0.0, 4.97068, 0.010344, -0.0192614, -0.0], [0.0, 0.236704, 0.010344, 0.714663, -0.000179083, 0.00768923], [-0.236704, 0.0, -0.0192614, -0.000179083, 0.717956, 0.0196616], [-0.010344, 0.0192614, 0.0, 0.00768923, 0.0196616, 0.00921316]]
        // JOINT 2:
        // [[0.646926, 0.0, 0.0, -0.0, 0.00226101, 0.0185797], [0.0, 0.646926, 0.0, -0.00226101, -0.0, -0.00203199], [0.0, 0.0, 0.646926, -0.0185797, 0.00203199, -0.0], [0.0, -0.00226101, -0.0185797, 0.00850351, -0.00398336, 0.0102611], [0.00226101, 0.0, 0.00203199, -0.00398336, 0.0281243, 0.000768936], [0.0185797, -0.00203199, 0.0, 0.0102611, 0.000768936, 0.026535]]
        // JOINT 3:
        // [[3.2286, 0.0, 0.0, -0.0, -0.214709, -0.126729], [0.0, 3.2286, 0.0, 0.214709, -0.0, 0.0888447], [0.0, 0.0, 3.2286, 0.126729, -0.0888447, -0.0], [0.0, 0.214709, 0.126729, 0.0564949, -0.00824833, -0.00548765], [-0.214709, 0.0, -0.0888447, -0.00824833, 0.0528784, -0.00437726], [-0.126729, 0.0888447, 0.0, -0.00548765, -0.00437726, 0.0182492]]
        // JOINT 4:
        // [
        //    [3.5879, 0.0, 0.0, -0.0, 0.0985021, -0.374644], 
        //    [0.0, 3.5879, 0.0, -0.0985021, -0.0, -0.190768], 
        //    [0.0, 0.0, 3.5879, 0.374644, 0.190768, -0.0], 
        //    [0.0, -0.0985021, 0.374644, 0.0676773, 0.0277158, 0.00390536], 
        //    [0.0985021, 0.0, 0.190768, 0.0277158, 0.0323994, -0.00164449],
        //    [-0.374644, -0.190768, 0.0, 0.00390536, -0.00164449, 0.0775861]]
        // JOINT 5:
        // [
        //    [1.22595, 0.0, 0.0, -0.0, -0.0471217, -0.0503435], 
        //    [0.0, 1.22595, 0.0, 0.0471217, -0.0, -0.0146537], 
        //    [0.0, 0.0, 1.22595, 0.0503435, 0.0146537, -0.0], 
        //    [0.0, 0.0471217, 0.0503435, 0.0394276, -0.00151524, -0.00460025], 
        //    [-0.0471217, 0.0, 0.0146537, -0.00151524, 0.0314604, 0.00216405], 
        //    [-0.0503435, -0.0146537, 0.0, -0.00460025, 0.00216405, 0.0108695]]
        // JOINT 6:
        // [
        //      [2.40208, 0.0, 0.0, -0.0, -0.0206546, 0.0688327], 
        //      [0.0, 2.40208, 0.0, 0.0206546, -0.0, 0.172703], 
        //      [0.0, 0.0, 2.40208, -0.0688327, -0.172703, -0.0], 
        //      [0.0, 0.0206546, -0.0688327, 0.0178005, 0.00718352, -0.000223653], 
        //      [-0.0206546, 0.0, -0.172703, 0.00718352, 0.0225347, 0.000641928], 
        //      [0.0688327, 0.172703, 0.0, -0.000223653, 0.000641928, 0.031751]]


        // Now continue with pass 2
        // This pass is backwards, so I'll start with joint 6

        // JOINT 6
        // data_U = Ia.col(5)
        // data_Dinv = 0(matrix)
        // data_Dinv[0] = 1 / Ia(5, 5)
        // data_UDinv = data_U * data_Dinv(0)
        // if parent > 0
        //    Ia -= data_UDinv * data_U.transpose()
        //    Yaba[parent] += sth sth
        // return data_Dinv(0)


        //val U_6_1 = 0.0688327
        //val U_6_2 = 0.172703
        //val U_6_3 = 0.0
        //val U_6_4 = -0.000223653
        //val U_6_5 = 0.000641928
        //val U_6_6 = 0.031751
        
        //val Dinv_6_1 = 1 / 0.031751 // 31.495

        //val UDinv_6_1 = U_6_1 * Dinv_6_1 // 2.168
        //val UDinv_6_2 = U_6_2 * Dinv_6_1 // 5.439
        //val UDinv_6_3 = U_6_3 * Dinv_6_1 // 0.0
        //val UDinv_6_4 = U_6_4 * Dinv_6_1 // -0.007
        //val UDinv_6_5 = U_6_5 * Dinv_6_1 // 0.020
        //val UDinv_6_6 = U_6_6 * Dinv_6_1 // 1.0

        // Ia -= UDinv * U.transpose()
        // I guess UDinv * U.transpose() is a 6x6 matrix
        //val Ia_6_1_1 = 2.40208 - UDinv_6_1 * U_6_1 // 2.253
        //val Ia_6_1_2 = 0.0 - UDinv_6_1 * U_6_2 // -0.374
        //val Ia_6_1_3 = 0.0 - UDinv_6_1 * U_6_3 // 0.0
        //val Ia_6_1_4 = 0.0 - UDinv_6_1 * U_6_4 // 0.0
        //val Ia_6_1_5 = -0.0206546 - UDinv_6_1 * U_6_5 // -0.022
        //val Ia_6_1_6 = 0.0688327 - UDinv_6_1 * U_6_6 // 0.0

        //val Ia_6_2_1 = 0.0 - UDinv_6_2 * U_6_1 // -0.374
        //val Ia_6_2_2 = 2.40208 - UDinv_6_2 * U_6_2 // 1.463
        //val Ia_6_2_3 = 0.0 - UDinv_6_2 * U_6_3 // 0.0
        //val Ia_6_2_4 = 0.0206546 - UDinv_6_2 * U_6_4 // 0.022
        //val Ia_6_2_5 = 0.0 - UDinv_6_2 * U_6_5 // -0.003
        //val Ia_6_2_6 = 0.172703 - UDinv_6_2 * U_6_6 // 0.0

        //val Ia_6_3_1 = 0.0 - UDinv_6_3 * U_6_1 // 0.0
        //val Ia_6_3_2 = 0.0 - UDinv_6_3 * U_6_2 // 0.0
        //val Ia_6_3_3 = 2.40208 - UDinv_6_3 * U_6_3 // 2.402
        //val Ia_6_3_4 = -0.0688327 - UDinv_6_3 * U_6_4 // -0.069
        //val Ia_6_3_5 = -0.172703 - UDinv_6_3 * U_6_5 // -0.173
        //val Ia_6_3_6 = 0.0 - UDinv_6_3 * U_6_6 // 0.0

        //val Ia_6_4_1 = 0.0 - UDinv_6_4 * U_6_1 // 0.0
        //val Ia_6_4_2 = 0.0206546 - UDinv_6_4 * U_6_2 // 0.022
        //val Ia_6_4_3 = -0.0688327 - UDinv_6_4 * U_6_3 // -0.069
        //val Ia_6_4_4 = 0.0178005 - UDinv_6_4 * U_6_4 // 0.018
        //val Ia_6_4_5 = 0.00718352 - UDinv_6_4 * U_6_5 // 0.007
        //val Ia_6_4_6 = -0.000223653 - UDinv_6_4 * U_6_6 // 0.0

        //val Ia_6_5_1 = -0.0206546 - UDinv_6_5 * U_6_1 // -0.022
        //val Ia_6_5_2 = 0.0 - UDinv_6_5 * U_6_2 // -0.003
        //val Ia_6_5_3 = -0.172703 - UDinv_6_5 * U_6_3 // -0.173
        //val Ia_6_5_4 = 0.00718352 - UDinv_6_5 * U_6_4 // 0.007
        //val Ia_6_5_5 = 0.0281243 - UDinv_6_5 * U_6_5 // 0.023
        //val Ia_6_5_6 = 0.000641928 - UDinv_6_5 * U_6_6 // 0.0

        //val Ia_6_6_1 = 0.0688327 - UDinv_6_6 * U_6_1 // 0.0
        //val Ia_6_6_2 = 0.172703 - UDinv_6_6 * U_6_2 // 0.0
        //val Ia_6_6_3 = 0.0 - UDinv_6_6 * U_6_3 // 0.0
        //val Ia_6_6_4 = -0.000223653 - UDinv_6_6 * U_6_4 // 0.0
        //val Ia_6_6_5 = 0.000641928 - UDinv_6_6 * U_6_5 // 0.0
        //val Ia_6_6_6 = 0.031751 - UDinv_6_6 * U_6_6 // 0.00

        //val Minv_6 = Dinv_6_1 // 31.495

        // Yaba[5] += acton(limi[6], Ia)
        // Ai = Ia[0:3][0:3]

        //val Ai_6_1_1 = Ia_6_1_1 // 2.253
        //val Ai_6_1_2 = Ia_6_1_2 // -0.374
        //val Ai_6_1_3 = Ia_6_1_3 // 0.0
        //val Ai_6_2_1 = Ia_6_2_1 // -0.374
        //val Ai_6_2_2 = Ia_6_2_2 // 1.463
        //val Ai_6_2_3 = Ia_6_2_3 // 0.0
        //val Ai_6_3_1 = Ia_6_3_1 // 0.0
        //val Ai_6_3_2 = Ia_6_3_2 // 0.0
        //val Ai_6_3_3 = Ia_6_3_3 // 2.402

        // Bi = Ia[0:3][3:6]
        //val Bi_6_1_1 = Ia_6_1_4 // 0.0
        //val Bi_6_1_2 = Ia_6_1_5 // -0.022
        //val Bi_6_1_3 = Ia_6_1_6 // 0.0
        //val Bi_6_2_1 = Ia_6_2_4 // 0.022
        //val Bi_6_2_2 = Ia_6_2_5 // -0.003
        //val Bi_6_2_3 = Ia_6_2_6 // 0.0
        //val Bi_6_3_1 = Ia_6_3_4 // -0.069
        //val Bi_6_3_2 = Ia_6_3_5 // -0.173
        //val Bi_6_3_3 = Ia_6_3_6 // 0.0

        // Ci = Ia[3:6][0:3]
        //val Ci_6_1_1 = Ia_6_4_1 // 0.0
        //val Ci_6_1_2 = Ia_6_4_2 // 0.022
        //val Ci_6_1_3 = Ia_6_4_3 // -0.069
        //val Ci_6_2_1 = Ia_6_5_1 // -0.022
        //val Ci_6_2_2 = Ia_6_5_2 // -0.003
        //val Ci_6_2_3 = Ia_6_5_3 // -0.173
        //val Ci_6_3_1 = Ia_6_6_1 // 0.0
        //val Ci_6_3_2 = Ia_6_6_2 // 0.0
        //val Ci_6_3_3 = Ia_6_6_3 // 0.0

        // Di = Ia[3:6][3:6]
        //val Di_6_1_1 = Ia_6_4_4 // 0.018
        //val Di_6_1_2 = Ia_6_4_5 // 0.007
        //val Di_6_1_3 = Ia_6_4_6 // 0.0
        //val Di_6_2_1 = Ia_6_5_4 // 0.007
        //val Di_6_2_2 = Ia_6_5_5 // 0.023
        //val Di_6_2_3 = Ia_6_5_6 // 0.0
        //val Di_6_3_1 = Ia_6_6_4 // 0.0
        //val Di_6_3_2 = Ia_6_6_5 // 0.0
        //val Di_6_3_3 = Ia_6_6_6 // 0.0

        // Ai, Bi, Ci and Di will be updated, and these values will update limi[6] and Ia
        // I am skipping Ia updates because this joint's Ia and limi will not be used anymore
        // TODO: This may change in future, BE CAREFUL!

        // a temp variable will be R * Ai
        //val temp1_6_1_1 = limi_rotation_6_1_1 * Ai_6_1_1 + limi_rotation_6_1_2 * Ai_6_2_1 + limi_rotation_6_1_3 * Ai_6_3_1
        //val temp1_6_1_2 = limi_rotation_6_1_1 * Ai_6_1_2 + limi_rotation_6_1_2 * Ai_6_2_2 + limi_rotation_6_1_3 * Ai_6_3_2
        //val temp1_6_1_3 = limi_rotation_6_1_1 * Ai_6_1_3 + limi_rotation_6_1_2 * Ai_6_2_3 + limi_rotation_6_1_3 * Ai_6_3_3
        //val temp1_6_2_1 = limi_rotation_6_2_1 * Ai_6_1_1 + limi_rotation_6_2_2 * Ai_6_2_1 + limi_rotation_6_2_3 * Ai_6_3_1
        //val temp1_6_2_2 = limi_rotation_6_2_1 * Ai_6_1_2 + limi_rotation_6_2_2 * Ai_6_2_2 + limi_rotation_6_2_3 * Ai_6_3_2
        //val temp1_6_2_3 = limi_rotation_6_2_1 * Ai_6_1_3 + limi_rotation_6_2_2 * Ai_6_2_3 + limi_rotation_6_2_3 * Ai_6_3_3
        //val temp1_6_3_1 = limi_rotation_6_3_1 * Ai_6_1_1 + limi_rotation_6_3_2 * Ai_6_2_1 + limi_rotation_6_3_3 * Ai_6_3_1
        //val temp1_6_3_2 = limi_rotation_6_3_1 * Ai_6_1_2 + limi_rotation_6_3_2 * Ai_6_2_2 + limi_rotation_6_3_3 * Ai_6_3_2
        //val temp1_6_3_3 = limi_rotation_6_3_1 * Ai_6_1_3 + limi_rotation_6_3_2 * Ai_6_2_3 + limi_rotation_6_3_3 * Ai_6_3_3

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
        
        //val Ao_6_1_1 = temp1_6_1_1 * limi_rotation_transpose_6_1_1 + temp1_6_1_2 * limi_rotation_transpose_6_2_1 + temp1_6_1_3 * limi_rotation_transpose_6_3_1
        //val Ao_6_1_2 = temp1_6_1_1 * limi_rotation_transpose_6_1_2 + temp1_6_1_2 * limi_rotation_transpose_6_2_2 + temp1_6_1_3 * limi_rotation_transpose_6_3_2
        //val Ao_6_1_3 = temp1_6_1_1 * limi_rotation_transpose_6_1_3 + temp1_6_1_2 * limi_rotation_transpose_6_2_3 + temp1_6_1_3 * limi_rotation_transpose_6_3_3
        //val Ao_6_2_1 = temp1_6_2_1 * limi_rotation_transpose_6_1_1 + temp1_6_2_2 * limi_rotation_transpose_6_2_1 + temp1_6_2_3 * limi_rotation_transpose_6_3_1
        //val Ao_6_2_2 = temp1_6_2_1 * limi_rotation_transpose_6_1_2 + temp1_6_2_2 * limi_rotation_transpose_6_2_2 + temp1_6_2_3 * limi_rotation_transpose_6_3_2
        //val Ao_6_2_3 = temp1_6_2_1 * limi_rotation_transpose_6_1_3 + temp1_6_2_2 * limi_rotation_transpose_6_2_3 + temp1_6_2_3 * limi_rotation_transpose_6_3_3
        //val Ao_6_3_1 = temp1_6_3_1 * limi_rotation_transpose_6_1_1 + temp1_6_3_2 * limi_rotation_transpose_6_2_1 + temp1_6_3_3 * limi_rotation_transpose_6_3_1
        //val Ao_6_3_2 = temp1_6_3_1 * limi_rotation_transpose_6_1_2 + temp1_6_3_2 * limi_rotation_transpose_6_2_2 + temp1_6_3_3 * limi_rotation_transpose_6_3_2
        //val Ao_6_3_3 = temp1_6_3_1 * limi_rotation_transpose_6_1_3 + temp1_6_3_2 * limi_rotation_transpose_6_2_3 + temp1_6_3_3 * limi_rotation_transpose_6_3_3

        val Ao_6_1_1 = temp1_6_1_1 * limi_rotation_transpose_6_1_1 + temp1_6_1_2 * limi_rotation_transpose_6_2_1
        //val Ao_6_1_2 = 0.0  
        val Ao_6_1_3 = temp1_6_1_1 * limi_rotation_transpose_6_1_3 + temp1_6_1_2 * limi_rotation_transpose_6_2_3
        //val Ao_6_2_1 = 0.0
        //val Ao_6_2_2 = 2.402 
        //val Ao_6_2_3 = 0.0
        val Ao_6_3_1 = temp1_6_3_1 * limi_rotation_transpose_6_1_1 + temp1_6_3_2 * limi_rotation_transpose_6_2_1
        //val Ao_6_3_2 = 0.0
        val Ao_6_3_3 = temp1_6_3_1 * limi_rotation_transpose_6_1_3 + temp1_6_3_2 * limi_rotation_transpose_6_2_3

        // a temp variable will be R * Bi, then Bo = temp2 * R.transpose()
        //val temp2_6_1_1 = limi_rotation_6_1_1 * Bi_6_1_1 + limi_rotation_6_1_2 * Bi_6_2_1 + limi_rotation_6_1_3 * Bi_6_3_1
        //val temp2_6_1_2 = limi_rotation_6_1_1 * Bi_6_1_2 + limi_rotation_6_1_2 * Bi_6_2_2 + limi_rotation_6_1_3 * Bi_6_3_2
        //val temp2_6_1_3 = limi_rotation_6_1_1 * Bi_6_1_3 + limi_rotation_6_1_2 * Bi_6_2_3 + limi_rotation_6_1_3 * Bi_6_3_3
        //val temp2_6_2_1 = limi_rotation_6_2_1 * Bi_6_1_1 + limi_rotation_6_2_2 * Bi_6_2_1 + limi_rotation_6_2_3 * Bi_6_3_1
        //val temp2_6_2_2 = limi_rotation_6_2_1 * Bi_6_1_2 + limi_rotation_6_2_2 * Bi_6_2_2 + limi_rotation_6_2_3 * Bi_6_3_2
        //val temp2_6_2_3 = limi_rotation_6_2_1 * Bi_6_1_3 + limi_rotation_6_2_2 * Bi_6_2_3 + limi_rotation_6_2_3 * Bi_6_3_3
        //val temp2_6_3_1 = limi_rotation_6_3_1 * Bi_6_1_1 + limi_rotation_6_3_2 * Bi_6_2_1 + limi_rotation_6_3_3 * Bi_6_3_1
        //val temp2_6_3_2 = limi_rotation_6_3_1 * Bi_6_1_2 + limi_rotation_6_3_2 * Bi_6_2_2 + limi_rotation_6_3_3 * Bi_6_3_2
        //val temp2_6_3_3 = limi_rotation_6_3_1 * Bi_6_1_3 + limi_rotation_6_3_2 * Bi_6_2_3 + limi_rotation_6_3_3 * Bi_6_3_3

        val temp2_6_1_1 = limi_rotation_6_1_2 * 0.022
        val temp2_6_1_2 = limi_rotation_6_1_1 * -0.022 + limi_rotation_6_1_2 * -0.003
        //val temp2_6_1_3 = 0.0
        //val temp2_6_2_1 = 0.069
        //val temp2_6_2_2 = 0.173
        //val temp2_6_2_3 = 0.0
        val temp2_6_3_1 = limi_rotation_6_3_2 * 0.022
        val temp2_6_3_2 = limi_rotation_6_3_1 * -0.022 + limi_rotation_6_3_2 * -0.003
        //val temp2_6_3_3 = 0.0

        // Now temp2 * R.transpose()
        //val Bo_temp_6_1_1 = temp2_6_1_1 * limi_rotation_transpose_6_1_1 + temp2_6_1_2 * limi_rotation_transpose_6_2_1 + temp2_6_1_3 * limi_rotation_transpose_6_3_1
        //val Bo_temp_6_1_2 = temp2_6_1_1 * limi_rotation_transpose_6_1_2 + temp2_6_1_2 * limi_rotation_transpose_6_2_2 + temp2_6_1_3 * limi_rotation_transpose_6_3_2
        //val Bo_temp_6_1_3 = temp2_6_1_1 * limi_rotation_transpose_6_1_3 + temp2_6_1_2 * limi_rotation_transpose_6_2_3 + temp2_6_1_3 * limi_rotation_transpose_6_3_3
        //val Bo_temp_6_2_1 = temp2_6_2_1 * limi_rotation_transpose_6_1_1 + temp2_6_2_2 * limi_rotation_transpose_6_2_1 + temp2_6_2_3 * limi_rotation_transpose_6_3_1
        //val Bo_temp_6_2_2 = temp2_6_2_1 * limi_rotation_transpose_6_1_2 + temp2_6_2_2 * limi_rotation_transpose_6_2_2 + temp2_6_2_3 * limi_rotation_transpose_6_3_2
        //val Bo_temp_6_2_3 = temp2_6_2_1 * limi_rotation_transpose_6_1_3 + temp2_6_2_2 * limi_rotation_transpose_6_2_3 + temp2_6_2_3 * limi_rotation_transpose_6_3_3
        //val Bo_temp_6_3_1 = temp2_6_3_1 * limi_rotation_transpose_6_1_1 + temp2_6_3_2 * limi_rotation_transpose_6_2_1 + temp2_6_3_3 * limi_rotation_transpose_6_3_1
        //val Bo_temp_6_3_2 = temp2_6_3_1 * limi_rotation_transpose_6_1_2 + temp2_6_3_2 * limi_rotation_transpose_6_2_2 + temp2_6_3_3 * limi_rotation_transpose_6_3_2
        //val Bo_temp_6_3_3 = temp2_6_3_1 * limi_rotation_transpose_6_1_3 + temp2_6_3_2 * limi_rotation_transpose_6_2_3 + temp2_6_3_3 * limi_rotation_transpose_6_3_3

        val Bo_temp_6_1_1 = temp2_6_1_1 * limi_rotation_transpose_6_1_1 + temp2_6_1_2 * limi_rotation_transpose_6_2_1
        //val Bo_6_1_2 = 0.0
        val Bo_temp_6_1_3 = temp2_6_1_1 * limi_rotation_transpose_6_1_3 + temp2_6_1_2 * limi_rotation_transpose_6_2_3
        val Bo_temp_6_2_1 = 0.069 * limi_rotation_transpose_6_1_1 + 0.173 * limi_rotation_transpose_6_2_1
        //val Bo_6_2_2 = 0.0
        val Bo_temp_6_2_3 = 0.069 * limi_rotation_transpose_6_1_3 + 0.173 * limi_rotation_transpose_6_2_3
        val Bo_temp_6_3_1 = temp2_6_3_1 * limi_rotation_transpose_6_1_1 + temp2_6_3_2 * limi_rotation_transpose_6_2_1
        //val Bo_temp_6_3_2 = 0.0
        val Bo_temp_6_3_3 = temp2_6_3_1 * limi_rotation_transpose_6_1_3 + temp2_6_3_2 * limi_rotation_transpose_6_2_3

        // a temp variable will be R * Di, then Do_temp = temp3 * R.transpose()
        //val temp3_6_1_1 = limi_rotation_6_1_1 * Di_6_1_1 + limi_rotation_6_1_2 * Di_6_2_1 + limi_rotation_6_1_3 * Di_6_3_1
        //val temp3_6_1_2 = limi_rotation_6_1_1 * Di_6_1_2 + limi_rotation_6_1_2 * Di_6_2_2 + limi_rotation_6_1_3 * Di_6_3_2
        //val temp3_6_1_3 = limi_rotation_6_1_1 * Di_6_1_3 + limi_rotation_6_1_2 * Di_6_2_3 + limi_rotation_6_1_3 * Di_6_3_3
        //val temp3_6_2_1 = limi_rotation_6_2_1 * Di_6_1_1 + limi_rotation_6_2_2 * Di_6_2_1 + limi_rotation_6_2_3 * Di_6_3_1
        //val temp3_6_2_2 = limi_rotation_6_2_1 * Di_6_1_2 + limi_rotation_6_2_2 * Di_6_2_2 + limi_rotation_6_2_3 * Di_6_3_2
        //val temp3_6_2_3 = limi_rotation_6_2_1 * Di_6_1_3 + limi_rotation_6_2_2 * Di_6_2_3 + limi_rotation_6_2_3 * Di_6_3_3
        //val temp3_6_3_1 = limi_rotation_6_3_1 * Di_6_1_1 + limi_rotation_6_3_2 * Di_6_2_1 + limi_rotation_6_3_3 * Di_6_3_1
        //val temp3_6_3_2 = limi_rotation_6_3_1 * Di_6_1_2 + limi_rotation_6_3_2 * Di_6_2_2 + limi_rotation_6_3_3 * Di_6_3_2
        //val temp3_6_3_3 = limi_rotation_6_3_1 * Di_6_1_3 + limi_rotation_6_3_2 * Di_6_2_3 + limi_rotation_6_3_3 * Di_6_3_3

        val temp3_6_1_1 = limi_rotation_6_1_1 * 0.018 + limi_rotation_6_1_2 * 0.007
        val temp3_6_1_2 = limi_rotation_6_1_1 * 0.007 + limi_rotation_6_1_2 * 0.023
        //val temp3_6_1_3 = 0.0
        //val temp3_6_2_1 = 0.0
        //val temp3_6_2_2 = 0.0
        //val temp3_6_2_3 = 0.0
        val temp3_6_3_1 = limi_rotation_6_3_1 * 0.018 + limi_rotation_6_3_2 * 0.007
        val temp3_6_3_2 = limi_rotation_6_3_1 * 0.007 + limi_rotation_6_3_2 * 0.023
        //val temp3_6_3_3 = 0.0

        // TODO: WRONG, CHECK JOINT 5
        //val Do_temp_6_1_1 = temp3_6_1_1 * limi_rotation_transpose_6_1_1 + temp3_6_1_2 * limi_rotation_transpose_6_2_1 + temp3_6_1_3 * limi_rotation_transpose_6_3_1
        //val Do_temp_6_1_2 = temp3_6_1_1 * limi_rotation_transpose_6_1_2 + temp3_6_1_2 * limi_rotation_transpose_6_2_2 + temp3_6_1_3 * limi_rotation_transpose_6_3_2
        //val Do_temp_6_1_3 = temp3_6_1_1 * limi_rotation_transpose_6_1_3 + temp3_6_1_2 * limi_rotation_transpose_6_2_3 + temp3_6_1_3 * limi_rotation_transpose_6_3_3
        //val Do_temp_6_2_1 = temp3_6_2_1 * limi_rotation_transpose_6_1_1 + temp3_6_2_2 * limi_rotation_transpose_6_2_1 + temp3_6_2_3 * limi_rotation_transpose_6_3_1
        //val Do_temp_6_2_2 = temp3_6_2_1 * limi_rotation_transpose_6_1_2 + temp3_6_2_2 * limi_rotation_transpose_6_2_2 + temp3_6_2_3 * limi_rotation_transpose_6_3_2
        //val Do_temp_6_2_3 = temp3_6_2_1 * limi_rotation_transpose_6_1_3 + temp3_6_2_2 * limi_rotation_transpose_6_2_3 + temp3_6_2_3 * limi_rotation_transpose_6_3_3
        //val Do_temp_6_3_1 = temp3_6_3_1 * limi_rotation_transpose_6_1_1 + temp3_6_3_2 * limi_rotation_transpose_6_2_1 + temp3_6_3_3 * limi_rotation_transpose_6_3_1
        //val Do_temp_6_3_2 = temp3_6_3_1 * limi_rotation_transpose_6_1_2 + temp3_6_3_2 * limi_rotation_transpose_6_2_2 + temp3_6_3_3 * limi_rotation_transpose_6_3_2
        //val Do_temp_6_3_3 = temp3_6_3_1 * limi_rotation_transpose_6_1_3 + temp3_6_3_2 * limi_rotation_transpose_6_2_3 + temp3_6_3_3 * limi_rotation_transpose_6_3_3

        val Do_temp_6_1_1 = temp3_6_1_1 * limi_rotation_transpose_6_1_1 + temp3_6_1_2 * limi_rotation_transpose_6_2_1
        //val Do_temp_6_1_2 = 0.0
        val Do_temp_6_1_3 = temp3_6_1_1 * limi_rotation_transpose_6_1_3 + temp3_6_1_2 * limi_rotation_transpose_6_2_3
        //val Do_temp_6_2_1 = 0.0
        //val Do_temp_6_2_2 = 0.0
        //val Do_temp_6_2_3 = 0.0
        val Do_temp_6_3_1 = temp3_6_3_1 * limi_rotation_transpose_6_1_1 + temp3_6_3_2 * limi_rotation_transpose_6_2_1
        //val Do_temp_6_3_2 = 0.0
        val Do_temp_6_3_3 = temp3_6_3_1 * limi_rotation_transpose_6_1_3 + temp3_6_3_2 * limi_rotation_transpose_6_2_3

        // Then, we'll have 
        // Do_temp2.row(1) = limi_translation.cross(Bo.col(1)) + Do_temp.row(1)
        // Do_temp2.row(2) = limi_translation.cross(Bo.col(2)) + Do_temp.row(2)
        // Do_temp2.row(3) = limi_translation.cross(Bo.col(3)) + Do_temp.row(3)
        //val Do_temp2_6_1_1 = limi_translation_6_2 * Bo_temp_6_3_1 - limi_translation_6_3 * Bo_temp_6_2_1 + Do_temp_6_1_1
        //val Do_temp2_6_1_2 = limi_translation_6_2 * Bo_temp_6_3_2 - limi_translation_6_3 * Bo_temp_6_2_2 + Do_temp_6_1_2
        //val Do_temp2_6_1_3 = limi_translation_6_2 * Bo_temp_6_3_3 - limi_translation_6_3 * Bo_temp_6_2_3 + Do_temp_6_1_3
        //val Do_temp2_6_2_1 = limi_translation_6_3 * Bo_temp_6_1_1 - limi_translation_6_1 * Bo_temp_6_3_1 + Do_temp_6_2_1
        //val Do_temp2_6_2_2 = limi_translation_6_3 * Bo_temp_6_1_2 - limi_translation_6_1 * Bo_temp_6_3_2 + Do_temp_6_2_2
        //val Do_temp2_6_2_3 = limi_translation_6_3 * Bo_temp_6_1_3 - limi_translation_6_1 * Bo_temp_6_3_3 + Do_temp_6_2_3
        //val Do_temp2_6_3_1 = limi_translation_6_1 * Bo_temp_6_2_1 - limi_translation_6_2 * Bo_temp_6_1_1 + Do_temp_6_3_1
        //val Do_temp2_6_3_2 = limi_translation_6_1 * Bo_temp_6_2_2 - limi_translation_6_2 * Bo_temp_6_1_2 + Do_temp_6_3_2
        //val Do_temp2_6_3_3 = limi_translation_6_1 * Bo_temp_6_2_3 - limi_translation_6_2 * Bo_temp_6_1_3 + Do_temp_6_3_3

        val Do_temp2_6_1_1 = Do_temp_6_1_1
        //val Do_temp2_6_1_2 = Do_temp_6_1_2 //0.0
        val Do_temp2_6_1_3 = Do_temp_6_1_3
        //val Do_temp2_6_2_1 = Do_temp_6_2_1 //0.0
        //val Do_temp2_6_2_2 = Do_temp_6_2_2 //0.0
        //val Do_temp2_6_2_3 = Do_temp_6_2_3 //0.0
        val Do_temp2_6_3_1 = Do_temp_6_3_1
        //val Do_temp2_6_3_2 = Do_temp_6_3_2 //0.0
        val Do_temp2_6_3_3 = Do_temp_6_3_3

        // Now, we'll have TODO: THIS IS WRONG, PLEASE REFER TO JOINT 5 Co
        // Co.col(1) = limi_translation.cross(Ao.col(1)) + Bo.col(1)
        // Co.col(2) = limi_translation.cross(Ao.col(2)) + Bo.col(2)
        // Co.col(3) = limi_translation.cross(Ao.col(3)) + Bo.col(3)
        //val Co_6_1_1 = limi_translation_6_2 * Ao_6_3_1 - limi_translation_6_3 * Ao_6_2_1 + Bo_temp_6_1_1
        //val Co_6_1_2 = limi_translation_6_2 * Ao_6_3_2 - limi_translation_6_3 * Ao_6_2_2 + Bo_temp_6_1_2
        //val Co_6_1_3 = limi_translation_6_2 * Ao_6_3_3 - limi_translation_6_3 * Ao_6_2_3 + Bo_temp_6_1_3
        //val Co_6_2_1 = limi_translation_6_3 * Ao_6_1_1 - limi_translation_6_1 * Ao_6_3_1 + Bo_temp_6_2_1
        //val Co_6_2_2 = limi_translation_6_3 * Ao_6_1_2 - limi_translation_6_1 * Ao_6_3_2 + Bo_temp_6_2_2
        //val Co_6_2_3 = limi_translation_6_3 * Ao_6_1_3 - limi_translation_6_1 * Ao_6_3_3 + Bo_temp_6_2_3
        //val Co_6_3_1 = limi_translation_6_1 * Ao_6_2_1 - limi_translation_6_2 * Ao_6_1_1 + Bo_temp_6_3_1
        //val Co_6_3_2 = limi_translation_6_1 * Ao_6_2_2 - limi_translation_6_2 * Ao_6_1_2 + Bo_temp_6_3_2
        //val Co_6_3_3 = limi_translation_6_1 * Ao_6_2_3 - limi_translation_6_2 * Ao_6_1_3 + Bo_temp_6_3_3

        val Co_6_1_1 = Bo_temp_6_1_1
        //val Co_6_1_2 = Bo_temp_6_1_2 // 0.0
        val Co_6_1_3 = Bo_temp_6_1_3
        val Co_6_2_1 = Bo_temp_6_2_1
        //val Co_6_2_2 = Bo_temp_6_2_2 // 0.0
        val Co_6_2_3 = Bo_temp_6_2_3
        val Co_6_3_1 = Bo_temp_6_3_1
        //val Co_6_3_2 = Bo_temp_6_3_2 // 0.0
        val Co_6_3_3 = Bo_temp_6_3_3

        // Now, we'll have Bo = Co.transpose()
        //val Bo_6_1_1 = Co_6_1_1
        //val Bo_6_1_2 = Co_6_2_1
        //val Bo_6_1_3 = Co_6_3_1
        //val Bo_6_2_1 = Co_6_1_2
        //val Bo_6_2_2 = Co_6_2_2
        //val Bo_6_2_3 = Co_6_3_2
        //val Bo_6_3_1 = Co_6_1_3
        //val Bo_6_3_2 = Co_6_2_3
        //val Bo_6_3_3 = Co_6_3_3

        val Bo_6_1_1 = Co_6_1_1
        val Bo_6_1_2 = Co_6_2_1
        val Bo_6_1_3 = Co_6_3_1
        //val Bo_6_2_1 = Co_6_1_2 // 0.0
        //val Bo_6_2_2 = Co_6_2_2 // 0.0
        //val Bo_6_2_3 = Co_6_3_2 // 0.0
        val Bo_6_3_1 = Co_6_1_3
        val Bo_6_3_2 = Co_6_2_3
        val Bo_6_3_3 = Co_6_3_3

        // TODO: WRONG, CHECK JOINT 5
        // Now, Do = Do_temp2 + limi_translation.cross(Bo)
        //val Do_6_1_1 = Do_temp2_6_1_1 + limi_translation_6_2 * Bo_6_3_1 - limi_translation_6_3 * Bo_6_2_1
        //val Do_6_1_2 = Do_temp2_6_1_2 + limi_translation_6_2 * Bo_6_3_2 - limi_translation_6_3 * Bo_6_2_2
        //val Do_6_1_3 = Do_temp2_6_1_3 + limi_translation_6_2 * Bo_6_3_3 - limi_translation_6_3 * Bo_6_2_3
        //val Do_6_2_1 = Do_temp2_6_2_1 + limi_translation_6_3 * Bo_6_1_1 - limi_translation_6_1 * Bo_6_3_1
        //val Do_6_2_2 = Do_temp2_6_2_2 + limi_translation_6_3 * Bo_6_1_2 - limi_translation_6_1 * Bo_6_3_2
        //val Do_6_2_3 = Do_temp2_6_2_3 + limi_translation_6_3 * Bo_6_1_3 - limi_translation_6_1 * Bo_6_3_3
        //val Do_6_3_1 = Do_temp2_6_3_1 + limi_translation_6_1 * Bo_6_2_1 - limi_translation_6_2 * Bo_6_1_1
        //val Do_6_3_2 = Do_temp2_6_3_2 + limi_translation_6_1 * Bo_6_2_2 - limi_translation_6_2 * Bo_6_1_2
        //val Do_6_3_3 = Do_temp2_6_3_3 + limi_translation_6_1 * Bo_6_2_3 - limi_translation_6_2 * Bo_6_1_3

        val Do_6_1_1 = Do_temp2_6_1_1
        //val Do_6_1_2 = Do_temp2_6_1_2 // 0.0
        val Do_6_1_3 = Do_temp2_6_1_3
        //val Do_6_2_1 = Do_temp2_6_2_1 // 0.0
        //val Do_6_2_2 = Do_temp2_6_2_2 // 0.0
        //val Do_6_2_3 = Do_temp2_6_2_3 // 0.0
        val Do_6_3_1 = Do_temp2_6_3_1
        //val Do_6_3_2 = Do_temp2_6_3_2 // 0.0
        val Do_6_3_3 = Do_temp2_6_3_3

        // Finally, add this to Yaba[5]
        //Yaba[5] += acton(limi[6], Ia)
        //val Ia_5_1_1 = Ao_6_1_1 + 1.22595
        //val Ia_5_1_2 = Ao_6_1_2 + 0.0
        //val Ia_5_1_3 = Ao_6_1_3 + 0.0
        //val Ia_5_2_1 = Ao_6_2_1 + 0.0
        //val Ia_5_2_2 = Ao_6_2_2 + 1.22595
        //val Ia_5_2_3 = Ao_6_2_3 + 0.0
        //val Ia_5_3_1 = Ao_6_3_1 + 0.0
        //val Ia_5_3_2 = Ao_6_3_2 + 0.0
        //val Ia_5_3_3 = Ao_6_3_3 + 1.22595
        //val Ia_5_4_1 = Bo_6_1_1 + 0.0
        //val Ia_5_4_2 = Bo_6_1_2 - 0.0471217
        //val Ia_5_4_3 = Bo_6_1_3 - 0.0503435
        //val Ia_5_5_1 = Bo_6_2_1 + 0.0471217
        //val Ia_5_5_2 = Bo_6_2_2 + 0.0
        //val Ia_5_5_3 = Bo_6_2_3 - 0.0146537
        //val Ia_5_6_1 = Bo_6_3_1 + 0.0503435
        //val Ia_5_6_2 = Bo_6_3_2 + 0.0146537
        //val Ia_5_6_3 = Bo_6_3_3 + 0.0
        //val Ia_5_1_4 = Co_6_1_1 + 0.0
        //val Ia_5_1_5 = Co_6_1_2 - 0.0471217
        //val Ia_5_1_6 = Co_6_1_3 - 0.0503435
        //val Ia_5_2_4 = Co_6_2_1 + 0.0471217
        //val Ia_5_2_5 = Co_6_2_2 + 0.0
        //val Ia_5_2_6 = Co_6_2_3 - 0.0146537
        //val Ia_5_3_4 = Co_6_3_1 + 0.0503435
        //val Ia_5_3_5 = Co_6_3_2 + 0.0146537
        //val Ia_5_3_6 = Co_6_3_3 + 0.0
        //val Ia_5_4_4 = Do_6_1_1 + 0.0394276
        //val Ia_5_4_5 = Do_6_1_2 - 0.00151524
        //val Ia_5_4_6 = Do_6_1_3 - 0.00460025
        //val Ia_5_5_4 = Do_6_2_1 + 0.00151524
        //val Ia_5_5_5 = Do_6_2_2 + 0.0314604
        //val Ia_5_5_6 = Do_6_2_3 - 0.00216405
        //val Ia_5_6_4 = Do_6_3_1 + 0.00460025
        //val Ia_5_6_5 = Do_6_3_2 + 0.00216405
        //val Ia_5_6_6 = Do_6_3_3 + 0.0108695

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
        
        val UDinv_5_1 = U_5_1 * Dinv_5_1
        val UDinv_5_2 = U_5_2 * Dinv_5_1
        val UDinv_5_3 = U_5_3 * Dinv_5_1
        val UDinv_5_4 = U_5_4 * Dinv_5_1
        val UDinv_5_5 = 0.00216405 * Dinv_5_1
        val UDinv_5_6 = U_5_6 * Dinv_5_1
        
        // Ia -= UDinv * U.transpose()
        //val Ia_new_5_1_1 = Ia_5_1_1 - UDinv_5_1 * U_5_1
        //val Ia_new_5_1_2 = Ia_5_1_2 - UDinv_5_1 * U_5_2
        //val Ia_new_5_1_3 = Ia_5_1_3 - UDinv_5_1 * U_5_3
        //val Ia_new_5_1_4 = Ia_5_1_4 - UDinv_5_1 * U_5_4
        //val Ia_new_5_1_5 = Ia_5_1_5 - UDinv_5_1 * U_5_5
        //val Ia_new_5_1_6 = Ia_5_1_6 - UDinv_5_1 * U_5_6
        //val Ia_new_5_2_1 = Ia_5_2_1 - UDinv_5_2 * U_5_1
        //val Ia_new_5_2_2 = Ia_5_2_2 - UDinv_5_2 * U_5_2
        //val Ia_new_5_2_3 = Ia_5_2_3 - UDinv_5_2 * U_5_3
        //val Ia_new_5_2_4 = Ia_5_2_4 - UDinv_5_2 * U_5_4
        //val Ia_new_5_2_5 = Ia_5_2_5 - UDinv_5_2 * U_5_5
        //val Ia_new_5_2_6 = Ia_5_2_6 - UDinv_5_2 * U_5_6
        //val Ia_new_5_3_1 = Ia_5_3_1 - UDinv_5_3 * U_5_1
        //val Ia_new_5_3_2 = Ia_5_3_2 - UDinv_5_3 * U_5_2
        //val Ia_new_5_3_3 = Ia_5_3_3 - UDinv_5_3 * U_5_3
        //val Ia_new_5_3_4 = Ia_5_3_4 - UDinv_5_3 * U_5_4
        //val Ia_new_5_3_5 = Ia_5_3_5 - UDinv_5_3 * U_5_5
        //val Ia_new_5_3_6 = Ia_5_3_6 - UDinv_5_3 * U_5_6
        //val Ia_new_5_4_1 = Ia_5_4_1 - UDinv_5_4 * U_5_1
        //val Ia_new_5_4_2 = Ia_5_4_2 - UDinv_5_4 * U_5_2
        //val Ia_new_5_4_3 = Ia_5_4_3 - UDinv_5_4 * U_5_3
        //val Ia_new_5_4_4 = Ia_5_4_4 - UDinv_5_4 * U_5_4
        //val Ia_new_5_4_5 = Ia_5_4_5 - UDinv_5_4 * U_5_5
        //val Ia_new_5_4_6 = Ia_5_4_6 - UDinv_5_4 * U_5_6
        //val Ia_new_5_5_1 = Ia_5_5_1 - UDinv_5_5 * U_5_1
        //val Ia_new_5_5_2 = Ia_5_5_2 - UDinv_5_5 * U_5_2
        //val Ia_new_5_5_3 = Ia_5_5_3 - UDinv_5_5 * U_5_3
        //val Ia_new_5_5_4 = Ia_5_5_4 - UDinv_5_5 * U_5_4
        //val Ia_new_5_5_5 = Ia_5_5_5 - UDinv_5_5 * U_5_5
        //val Ia_new_5_5_6 = Ia_5_5_6 - UDinv_5_5 * U_5_6
        //val Ia_new_5_6_1 = Ia_5_6_1 - UDinv_5_6 * U_5_1
        //val Ia_new_5_6_2 = Ia_5_6_2 - UDinv_5_6 * U_5_2
        //val Ia_new_5_6_3 = Ia_5_6_3 - UDinv_5_6 * U_5_3
        //val Ia_new_5_6_4 = Ia_5_6_4 - UDinv_5_6 * U_5_4
        //val Ia_new_5_6_5 = Ia_5_6_5 - UDinv_5_6 * U_5_5
        //val Ia_new_5_6_6 = Ia_5_6_6 - UDinv_5_6 * U_5_6

        val Ia_new_5_1_1 = Ia_5_1_1 - UDinv_5_1 * U_5_1
        val Ia_new_5_1_2 = -UDinv_5_1 * U_5_2
        val Ia_new_5_1_3 = Ia_5_1_3 - UDinv_5_1 * U_5_3
        val Ia_new_5_1_4 = Ia_5_1_4 - UDinv_5_1 * U_5_4
        val Ia_new_5_1_5 = -0.0471217 - UDinv_5_1 * 0.00216405
        val Ia_new_5_1_6 = Ia_5_1_6 - UDinv_5_1 * U_5_6
        val Ia_new_5_2_1 = -UDinv_5_2 * U_5_1
        val Ia_new_5_2_2 = 3.62795 - UDinv_5_2 * U_5_2
        val Ia_new_5_2_3 = -UDinv_5_2 * U_5_3
        val Ia_new_5_2_4 = Ia_5_2_4 - UDinv_5_2 * U_5_4
        val Ia_new_5_2_5 = -UDinv_5_2 * 0.00216405
        val Ia_new_5_2_6 = Ia_5_2_6 - UDinv_5_2 * U_5_6
        val Ia_new_5_3_1 = Ia_5_3_1 - UDinv_5_3 * U_5_1
        val Ia_new_5_3_2 = -UDinv_5_3 * U_5_2
        val Ia_new_5_3_3 = Ia_5_3_3 - UDinv_5_3 * U_5_3
        val Ia_new_5_3_4 = Ia_5_3_4 - UDinv_5_3 * U_5_4
        val Ia_new_5_3_5 = 0.0156537 - UDinv_5_3 * 0.00216405
        val Ia_new_5_3_6 = Ia_5_3_6 - UDinv_5_3 * U_5_6
        val Ia_new_5_4_1 = Ia_5_4_1 - UDinv_5_4 * U_5_1
        val Ia_new_5_4_2 = Ia_5_4_2 - UDinv_5_4 * U_5_2
        val Ia_new_5_4_3 = Ia_5_4_3 - UDinv_5_4 * U_5_3
        val Ia_new_5_4_4 = Ia_5_4_4 - UDinv_5_4 * U_5_4
        val Ia_new_5_4_5 = -0.00151524 - UDinv_5_4 * 0.00216405
        val Ia_new_5_4_6 = Ia_5_4_6 - UDinv_5_4 * U_5_6
        val Ia_new_5_5_1 = 0.0471217 - UDinv_5_5 * U_5_1
        val Ia_new_5_5_2 = 0.0 - UDinv_5_5 * U_5_2
        val Ia_new_5_5_3 = 0.0146537 - UDinv_5_5 * U_5_3
        val Ia_new_5_5_4 = 0.00151524 - UDinv_5_5 * U_5_4
        val Ia_new_5_5_5 = 0.0314604 - UDinv_5_5 * 0.00216405
        val Ia_new_5_5_6 = -0.00216405 - UDinv_5_5 * U_5_6
        val Ia_new_5_6_1 = Ia_5_6_1 - UDinv_5_6 * U_5_1
        val Ia_new_5_6_2 = Ia_5_6_2 - UDinv_5_6 * U_5_2
        val Ia_new_5_6_3 = Ia_5_6_3 - UDinv_5_6 * U_5_3
        val Ia_new_5_6_4 = Ia_5_6_4 - UDinv_5_6 * U_5_4
        val Ia_new_5_6_5 = 0.00216405 - UDinv_5_6 * 0.00216405
        val Ia_new_5_6_6 = Ia_5_6_6 - UDinv_5_6 * U_5_6

        
        // Yaba[4] += acton(limi[5], Ia)
        // Ai = Ia[0:3][0:3]
        
        val Ai_5_1_1 = Ia_new_5_1_1
        val Ai_5_1_2 = Ia_new_5_1_2
        val Ai_5_1_3 = Ia_new_5_1_3
        val Ai_5_2_1 = Ia_new_5_2_1
        val Ai_5_2_2 = Ia_new_5_2_2
        val Ai_5_2_3 = Ia_new_5_2_3
        val Ai_5_3_1 = Ia_new_5_3_1
        val Ai_5_3_2 = Ia_new_5_3_2
        val Ai_5_3_3 = Ia_new_5_3_3
        
        // Bi = Ia[0:3][3:6]
        val Bi_5_1_1 = Ia_new_5_1_4 
        val Bi_5_1_2 = Ia_new_5_1_5 
        val Bi_5_1_3 = Ia_new_5_1_6 
        val Bi_5_2_1 = Ia_new_5_2_4 
        val Bi_5_2_2 = Ia_new_5_2_5 
        val Bi_5_2_3 = Ia_new_5_2_6 
        val Bi_5_3_1 = Ia_new_5_3_4 
        val Bi_5_3_2 = Ia_new_5_3_5 
        val Bi_5_3_3 = Ia_new_5_3_6 

        // Ci = Ia[3:6][0:3]
        val Ci_5_1_1 = Ia_new_5_4_1
        val Ci_5_1_2 = Ia_new_5_4_2
        val Ci_5_1_3 = Ia_new_5_4_3
        val Ci_5_2_1 = Ia_new_5_5_1
        val Ci_5_2_2 = Ia_new_5_5_2
        val Ci_5_2_3 = Ia_new_5_5_3
        val Ci_5_3_1 = Ia_new_5_6_1
        val Ci_5_3_2 = Ia_new_5_6_2
        val Ci_5_3_3 = Ia_new_5_6_3

        // Di = Ia[3:6][3:6]
        val Di_5_1_1 = Ia_new_5_4_4
        val Di_5_1_2 = Ia_new_5_4_5
        val Di_5_1_3 = Ia_new_5_4_6
        val Di_5_2_1 = Ia_new_5_5_4
        val Di_5_2_2 = Ia_new_5_5_5
        val Di_5_2_3 = Ia_new_5_5_6
        val Di_5_3_1 = Ia_new_5_6_4
        val Di_5_3_2 = Ia_new_5_6_5
        val Di_5_3_3 = Ia_new_5_6_6

        // Ai, Bi, Ci and Di will be updated, and these values will update limi[6] and Ia
        // I am skipping Ia updates because this joint's Ia and limi will not be used anymore
        // TODO: This may change in future, BE CAREFUL!

        // a temp variable will be R * Ai
        //val temp1_5_1_1 = limi_rotation_5_1_1 * Ai_5_1_1 + limi_rotation_5_1_2 * Ai_5_2_1 + limi_rotation_5_1_3 * Ai_5_3_1
        //val temp1_5_1_2 = limi_rotation_5_1_1 * Ai_5_1_2 + limi_rotation_5_1_2 * Ai_5_2_2 + limi_rotation_5_1_3 * Ai_5_3_2
        //val temp1_5_1_3 = limi_rotation_5_1_1 * Ai_5_1_3 + limi_rotation_5_1_2 * Ai_5_2_3 + limi_rotation_5_1_3 * Ai_5_3_3
        //val temp1_5_2_1 = limi_rotation_5_2_1 * Ai_5_1_1 + limi_rotation_5_2_2 * Ai_5_2_1 + limi_rotation_5_2_3 * Ai_5_3_1
        //val temp1_5_2_2 = limi_rotation_5_2_1 * Ai_5_1_2 + limi_rotation_5_2_2 * Ai_5_2_2 + limi_rotation_5_2_3 * Ai_5_3_2
        //val temp1_5_2_3 = limi_rotation_5_2_1 * Ai_5_1_3 + limi_rotation_5_2_2 * Ai_5_2_3 + limi_rotation_5_2_3 * Ai_5_3_3
        //val temp1_5_3_1 = limi_rotation_5_3_1 * Ai_5_1_1 + limi_rotation_5_3_2 * Ai_5_2_1 + limi_rotation_5_3_3 * Ai_5_3_1
        //val temp1_5_3_2 = limi_rotation_5_3_1 * Ai_5_1_2 + limi_rotation_5_3_2 * Ai_5_2_2 + limi_rotation_5_3_3 * Ai_5_3_2
        //val temp1_5_3_3 = limi_rotation_5_3_1 * Ai_5_1_3 + limi_rotation_5_3_2 * Ai_5_2_3 + limi_rotation_5_3_3 * Ai_5_3_3

        val temp1_5_1_1 = limi_rotation_5_1_1 * Ai_5_1_1 + limi_rotation_5_1_2 * Ai_5_2_1
        val temp1_5_1_2 = limi_rotation_5_1_1 * Ai_5_1_2 + limi_rotation_5_1_2 * Ai_5_2_2
        val temp1_5_1_3 = limi_rotation_5_1_1 * Ai_5_1_3 + limi_rotation_5_1_2 * Ai_5_2_3
        val temp1_5_2_1 = Ai_5_3_1
        val temp1_5_2_2 = Ai_5_3_2
        val temp1_5_2_3 = Ai_5_3_3
        val temp1_5_3_1 = limi_rotation_5_3_1 * Ai_5_1_1 + limi_rotation_5_3_2 * Ai_5_2_1
        val temp1_5_3_2 = limi_rotation_5_3_1 * Ai_5_1_2 + limi_rotation_5_3_2 * Ai_5_2_2
        val temp1_5_3_3 = limi_rotation_5_3_1 * Ai_5_1_3 + limi_rotation_5_3_2 * Ai_5_2_3
        

        // now Ao = temp1 * R.transpose()
        //val limi_rotation_transpose_5_1_1 = limi_rotation_5_1_1
        //val limi_rotation_transpose_5_1_2 = limi_rotation_5_2_1
        //val limi_rotation_transpose_5_1_3 = limi_rotation_5_3_1
        //val limi_rotation_transpose_5_2_1 = limi_rotation_5_1_2
        //val limi_rotation_transpose_5_2_2 = limi_rotation_5_2_2
        //val limi_rotation_transpose_5_2_3 = limi_rotation_5_3_2
        //val limi_rotation_transpose_5_3_1 = limi_rotation_5_1_3
        //val limi_rotation_transpose_5_3_2 = limi_rotation_5_2_3
        //val limi_rotation_transpose_5_3_3 = limi_rotation_5_3_3

        val limi_rotation_transpose_5_1_1 = limi_rotation_5_1_1
        //val limi_rotation_transpose_5_1_2 = 0.0
        val limi_rotation_transpose_5_1_3 = limi_rotation_5_3_1
        val limi_rotation_transpose_5_2_1 = limi_rotation_5_1_2
        //val limi_rotation_transpose_5_2_2 = 0.0
        val limi_rotation_transpose_5_2_3 = limi_rotation_5_3_2
        //val limi_rotation_transpose_5_3_1 = 0.0
        //val limi_rotation_transpose_5_3_2 = 1.0
        //val limi_rotation_transpose_5_3_3 = 0.0
        
        //val Ao_5_1_1 = temp1_5_1_1 * limi_rotation_transpose_5_1_1 + temp1_5_1_2 * limi_rotation_transpose_5_2_1 + temp1_5_1_3 * limi_rotation_transpose_5_3_1
        //val Ao_5_1_2 = temp1_5_1_1 * limi_rotation_transpose_5_1_2 + temp1_5_1_2 * limi_rotation_transpose_5_2_2 + temp1_5_1_3 * limi_rotation_transpose_5_3_2
        //val Ao_5_1_3 = temp1_5_1_1 * limi_rotation_transpose_5_1_3 + temp1_5_1_2 * limi_rotation_transpose_5_2_3 + temp1_5_1_3 * limi_rotation_transpose_5_3_3
        //val Ao_5_2_1 = temp1_5_2_1 * limi_rotation_transpose_5_1_1 + temp1_5_2_2 * limi_rotation_transpose_5_2_1 + temp1_5_2_3 * limi_rotation_transpose_5_3_1
        //val Ao_5_2_2 = temp1_5_2_1 * limi_rotation_transpose_5_1_2 + temp1_5_2_2 * limi_rotation_transpose_5_2_2 + temp1_5_2_3 * limi_rotation_transpose_5_3_2
        //val Ao_5_2_3 = temp1_5_2_1 * limi_rotation_transpose_5_1_3 + temp1_5_2_2 * limi_rotation_transpose_5_2_3 + temp1_5_2_3 * limi_rotation_transpose_5_3_3
        //val Ao_5_3_1 = temp1_5_3_1 * limi_rotation_transpose_5_1_1 + temp1_5_3_2 * limi_rotation_transpose_5_2_1 + temp1_5_3_3 * limi_rotation_transpose_5_3_1
        //val Ao_5_3_2 = temp1_5_3_1 * limi_rotation_transpose_5_1_2 + temp1_5_3_2 * limi_rotation_transpose_5_2_2 + temp1_5_3_3 * limi_rotation_transpose_5_3_2
        //val Ao_5_3_3 = temp1_5_3_1 * limi_rotation_transpose_5_1_3 + temp1_5_3_2 * limi_rotation_transpose_5_2_3 + temp1_5_3_3 * limi_rotation_transpose_5_3_3

        val Ao_5_1_1 = temp1_5_1_1 * limi_rotation_transpose_5_1_1 + temp1_5_1_2 * limi_rotation_transpose_5_2_1
        val Ao_5_1_2 = temp1_5_1_3
        val Ao_5_1_3 = temp1_5_1_1 * limi_rotation_transpose_5_1_3 + temp1_5_1_2 * limi_rotation_transpose_5_2_3
        val Ao_5_2_1 = temp1_5_2_1 * limi_rotation_transpose_5_1_1 + temp1_5_2_2 * limi_rotation_transpose_5_2_1
        val Ao_5_2_2 = temp1_5_2_3
        val Ao_5_2_3 = temp1_5_2_1 * limi_rotation_transpose_5_1_3 + temp1_5_2_2 * limi_rotation_transpose_5_2_3
        val Ao_5_3_1 = temp1_5_3_1 * limi_rotation_transpose_5_1_1 + temp1_5_3_2 * limi_rotation_transpose_5_2_1
        val Ao_5_3_2 = temp1_5_3_3
        val Ao_5_3_3 = temp1_5_3_1 * limi_rotation_transpose_5_1_3 + temp1_5_3_2 * limi_rotation_transpose_5_2_3

        // a temp variable will be R * Bi, then Bo = temp2 * R.transpose()
        //val temp2_6_1_1 = limi_rotation_6_1_1 * Bi_6_1_1 + limi_rotation_6_1_2 * Bi_6_2_1 + limi_rotation_6_1_3 * Bi_6_3_1
        //val temp2_6_1_2 = limi_rotation_6_1_1 * Bi_6_1_2 + limi_rotation_6_1_2 * Bi_6_2_2 + limi_rotation_6_1_3 * Bi_6_3_2
        //val temp2_6_1_3 = limi_rotation_6_1_1 * Bi_6_1_3 + limi_rotation_6_1_2 * Bi_6_2_3 + limi_rotation_6_1_3 * Bi_6_3_3
        //val temp2_6_2_1 = limi_rotation_6_2_1 * Bi_6_1_1 + limi_rotation_6_2_2 * Bi_6_2_1 + limi_rotation_6_2_3 * Bi_6_3_1
        //val temp2_6_2_2 = limi_rotation_6_2_1 * Bi_6_1_2 + limi_rotation_6_2_2 * Bi_6_2_2 + limi_rotation_6_2_3 * Bi_6_3_2
        //val temp2_6_2_3 = limi_rotation_6_2_1 * Bi_6_1_3 + limi_rotation_6_2_2 * Bi_6_2_3 + limi_rotation_6_2_3 * Bi_6_3_3
        //val temp2_6_3_1 = limi_rotation_6_3_1 * Bi_6_1_1 + limi_rotation_6_3_2 * Bi_6_2_1 + limi_rotation_6_3_3 * Bi_6_3_1
        //val temp2_6_3_2 = limi_rotation_6_3_1 * Bi_6_1_2 + limi_rotation_6_3_2 * Bi_6_2_2 + limi_rotation_6_3_3 * Bi_6_3_2
        //val temp2_6_3_3 = limi_rotation_6_3_1 * Bi_6_1_3 + limi_rotation_6_3_2 * Bi_6_2_3 + limi_rotation_6_3_3 * Bi_6_3_3

        val temp2_5_1_1 = limi_rotation_5_1_1 * Bi_5_1_1 + limi_rotation_5_1_2 * Bi_5_2_1
        val temp2_5_1_2 = limi_rotation_5_1_1 * Bi_5_1_2 + limi_rotation_5_1_2 * Bi_5_2_2
        val temp2_5_1_3 = limi_rotation_5_1_1 * Bi_5_1_3 + limi_rotation_5_1_2 * Bi_5_2_3
        val temp2_5_2_1 = Bi_5_3_1
        val temp2_5_2_2 = Bi_5_3_2
        val temp2_5_2_3 = Bi_5_3_3
        val temp2_5_3_1 = limi_rotation_5_3_1 * Bi_5_1_1 + limi_rotation_5_3_2 * Bi_5_2_1
        val temp2_5_3_2 = limi_rotation_5_3_1 * Bi_5_1_2 + limi_rotation_5_3_2 * Bi_5_2_2
        val temp2_5_3_3 = limi_rotation_5_3_1 * Bi_5_1_3 + limi_rotation_5_3_2 * Bi_5_2_3

        // Now temp2 * R.transpose()
        //val Bo_temp_6_1_1 = temp2_6_1_1 * limi_rotation_transpose_6_1_1 + temp2_6_1_2 * limi_rotation_transpose_6_2_1 + temp2_6_1_3 * limi_rotation_transpose_6_3_1
        //val Bo_temp_6_1_2 = temp2_6_1_1 * limi_rotation_transpose_6_1_2 + temp2_6_1_2 * limi_rotation_transpose_6_2_2 + temp2_6_1_3 * limi_rotation_transpose_6_3_2
        //val Bo_temp_6_1_3 = temp2_6_1_1 * limi_rotation_transpose_6_1_3 + temp2_6_1_2 * limi_rotation_transpose_6_2_3 + temp2_6_1_3 * limi_rotation_transpose_6_3_3
        //val Bo_temp_6_2_1 = temp2_6_2_1 * limi_rotation_transpose_6_1_1 + temp2_6_2_2 * limi_rotation_transpose_6_2_1 + temp2_6_2_3 * limi_rotation_transpose_6_3_1
        //val Bo_temp_6_2_2 = temp2_6_2_1 * limi_rotation_transpose_6_1_2 + temp2_6_2_2 * limi_rotation_transpose_6_2_2 + temp2_6_2_3 * limi_rotation_transpose_6_3_2
        //val Bo_temp_6_2_3 = temp2_6_2_1 * limi_rotation_transpose_6_1_3 + temp2_6_2_2 * limi_rotation_transpose_6_2_3 + temp2_6_2_3 * limi_rotation_transpose_6_3_3
        //val Bo_temp_6_3_1 = temp2_6_3_1 * limi_rotation_transpose_6_1_1 + temp2_6_3_2 * limi_rotation_transpose_6_2_1 + temp2_6_3_3 * limi_rotation_transpose_6_3_1
        //val Bo_temp_6_3_2 = temp2_6_3_1 * limi_rotation_transpose_6_1_2 + temp2_6_3_2 * limi_rotation_transpose_6_2_2 + temp2_6_3_3 * limi_rotation_transpose_6_3_2
        //val Bo_temp_6_3_3 = temp2_6_3_1 * limi_rotation_transpose_6_1_3 + temp2_6_3_2 * limi_rotation_transpose_6_2_3 + temp2_6_3_3 * limi_rotation_transpose_6_3_3

        val Bo_temp_5_1_1 = temp2_5_1_1 * limi_rotation_transpose_5_1_1 + temp2_5_1_2 * limi_rotation_transpose_5_2_1
        val Bo_temp_5_1_2 = temp2_5_1_3
        val Bo_temp_5_1_3 = temp2_5_1_1 * limi_rotation_transpose_5_1_3 + temp2_5_1_2 * limi_rotation_transpose_5_2_3
        val Bo_temp_5_2_1 = temp2_5_2_1 * limi_rotation_transpose_5_1_1 + temp2_5_2_2 * limi_rotation_transpose_5_2_1
        val Bo_temp_5_2_2 = temp2_5_2_3
        val Bo_temp_5_2_3 = temp2_5_2_1 * limi_rotation_transpose_5_1_3 + temp2_5_2_2 * limi_rotation_transpose_5_2_3
        val Bo_temp_5_3_1 = temp2_5_3_1 * limi_rotation_transpose_5_1_1 + temp2_5_3_2 * limi_rotation_transpose_5_2_1
        val Bo_temp_5_3_2 = temp2_5_3_3
        val Bo_temp_5_3_3 = temp2_5_3_1 * limi_rotation_transpose_5_1_3 + temp2_5_3_2 * limi_rotation_transpose_5_2_3

        // a temp variable will be R * Di, then Do_temp = temp3 * R.transpose()
        //val temp3_6_1_1 = limi_rotation_6_1_1 * Di_6_1_1 + limi_rotation_6_1_2 * Di_6_2_1 + limi_rotation_6_1_3 * Di_6_3_1
        //val temp3_6_1_2 = limi_rotation_6_1_1 * Di_6_1_2 + limi_rotation_6_1_2 * Di_6_2_2 + limi_rotation_6_1_3 * Di_6_3_2
        //val temp3_6_1_3 = limi_rotation_6_1_1 * Di_6_1_3 + limi_rotation_6_1_2 * Di_6_2_3 + limi_rotation_6_1_3 * Di_6_3_3
        //val temp3_6_2_1 = limi_rotation_6_2_1 * Di_6_1_1 + limi_rotation_6_2_2 * Di_6_2_1 + limi_rotation_6_2_3 * Di_6_3_1
        //val temp3_6_2_2 = limi_rotation_6_2_1 * Di_6_1_2 + limi_rotation_6_2_2 * Di_6_2_2 + limi_rotation_6_2_3 * Di_6_3_2
        //val temp3_6_2_3 = limi_rotation_6_2_1 * Di_6_1_3 + limi_rotation_6_2_2 * Di_6_2_3 + limi_rotation_6_2_3 * Di_6_3_3
        //val temp3_6_3_1 = limi_rotation_6_3_1 * Di_6_1_1 + limi_rotation_6_3_2 * Di_6_2_1 + limi_rotation_6_3_3 * Di_6_3_1
        //val temp3_6_3_2 = limi_rotation_6_3_1 * Di_6_1_2 + limi_rotation_6_3_2 * Di_6_2_2 + limi_rotation_6_3_3 * Di_6_3_2
        //val temp3_6_3_3 = limi_rotation_6_3_1 * Di_6_1_3 + limi_rotation_6_3_2 * Di_6_2_3 + limi_rotation_6_3_3 * Di_6_3_3

        val temp3_5_1_1 = limi_rotation_5_1_1 * Di_5_1_1 + limi_rotation_5_1_2 * Di_5_2_1
        val temp3_5_1_2 = limi_rotation_5_1_1 * Di_5_1_2 + limi_rotation_5_1_2 * Di_5_2_2
        val temp3_5_1_3 = limi_rotation_5_1_1 * Di_5_1_3 + limi_rotation_5_1_2 * Di_5_2_3
        val temp3_5_2_1 = Di_5_3_1
        val temp3_5_2_2 = Di_5_3_2
        val temp3_5_2_3 = Di_5_3_3
        val temp3_5_3_1 = limi_rotation_5_3_1 * Di_5_1_1 + limi_rotation_5_3_2 * Di_5_2_1
        val temp3_5_3_2 = limi_rotation_5_3_1 * Di_5_1_2 + limi_rotation_5_3_2 * Di_5_2_2
        val temp3_5_3_3 = limi_rotation_5_3_1 * Di_5_1_3 + limi_rotation_5_3_2 * Di_5_2_3

        //val Do_temp_6_1_1 = temp3_6_1_1 * limi_rotation_transpose_6_1_1 + temp3_6_1_2 * limi_rotation_transpose_6_2_1 + temp3_6_1_3 * limi_rotation_transpose_6_3_1
        //val Do_temp_6_1_2 = temp3_6_1_1 * limi_rotation_transpose_6_1_2 + temp3_6_1_2 * limi_rotation_transpose_6_2_2 + temp3_6_1_3 * limi_rotation_transpose_6_3_2
        //val Do_temp_6_1_3 = temp3_6_1_1 * limi_rotation_transpose_6_1_3 + temp3_6_1_2 * limi_rotation_transpose_6_2_3 + temp3_6_1_3 * limi_rotation_transpose_6_3_3
        //val Do_temp_6_2_1 = temp3_6_2_1 * limi_rotation_transpose_6_1_1 + temp3_6_2_2 * limi_rotation_transpose_6_2_1 + temp3_6_2_3 * limi_rotation_transpose_6_3_1
        //val Do_temp_6_2_2 = temp3_6_2_1 * limi_rotation_transpose_6_1_2 + temp3_6_2_2 * limi_rotation_transpose_6_2_2 + temp3_6_2_3 * limi_rotation_transpose_6_3_2
        //val Do_temp_6_2_3 = temp3_6_2_1 * limi_rotation_transpose_6_1_3 + temp3_6_2_2 * limi_rotation_transpose_6_2_3 + temp3_6_2_3 * limi_rotation_transpose_6_3_3
        //val Do_temp_6_3_1 = temp3_6_3_1 * limi_rotation_transpose_6_1_1 + temp3_6_3_2 * limi_rotation_transpose_6_2_1 + temp3_6_3_3 * limi_rotation_transpose_6_3_1
        //val Do_temp_6_3_2 = temp3_6_3_1 * limi_rotation_transpose_6_1_2 + temp3_6_3_2 * limi_rotation_transpose_6_2_2 + temp3_6_3_3 * limi_rotation_transpose_6_3_2
        //val Do_temp_6_3_3 = temp3_6_3_1 * limi_rotation_transpose_6_1_3 + temp3_6_3_2 * limi_rotation_transpose_6_2_3 + temp3_6_3_3 * limi_rotation_transpose_6_3_3

        val Do_temp_5_1_1 = temp3_5_1_1 * limi_rotation_transpose_5_1_1 + temp3_5_1_2 * limi_rotation_transpose_5_2_1
        val Do_temp_5_1_2 = temp3_5_1_3
        val Do_temp_5_1_3 = temp3_5_1_1 * limi_rotation_transpose_5_1_3 + temp3_5_1_2 * limi_rotation_transpose_5_2_3
        val Do_temp_5_2_1 = temp3_5_2_1 * limi_rotation_transpose_5_1_1 + temp3_5_2_2 * limi_rotation_transpose_5_2_1
        val Do_temp_5_2_2 = temp3_5_2_3
        val Do_temp_5_2_3 = temp3_5_2_1 * limi_rotation_transpose_5_1_3 + temp3_5_2_2 * limi_rotation_transpose_5_2_3
        val Do_temp_5_3_1 = temp3_5_3_1 * limi_rotation_transpose_5_1_1 + temp3_5_3_2 * limi_rotation_transpose_5_2_1
        val Do_temp_5_3_2 = temp3_5_3_3
        val Do_temp_5_3_3 = temp3_5_3_1 * limi_rotation_transpose_5_1_3 + temp3_5_3_2 * limi_rotation_transpose_5_2_3

        // Then, we'll have 
        // Do_temp2.row(1) = limi_translation.cross(Bo_temp.col(1)) + Do_temp.row(1)
        // Do_temp2.row(2) = limi_translation.cross(Bo_temp.col(2)) + Do_temp.row(2)
        // Do_temp2.row(3) = limi_translation.cross(Bo_temp.col(3)) + Do_temp.row(3)
        //val Do_temp2_5_1_1 = limi_translation_5_2 * Bo_temp_5_1_3 - limi_translation_5_3 * Bo_temp_5_1_2 + Do_temp_5_1_1
        //val Do_temp2_5_2_1 = limi_translation_5_3 * Bo_temp_5_1_1 - limi_translation_5_1 * Bo_temp_5_1_3 + Do_temp_5_2_1
        //val Do_temp2_5_3_1 = limi_translation_5_1 * Bo_temp_5_1_2 - limi_translation_5_2 * Bo_temp_5_1_1 + Do_temp_5_3_1
        //val Do_temp2_5_1_2 = limi_translation_5_2 * Bo_temp_5_2_3 - limi_translation_5_3 * Bo_temp_5_2_2 + Do_temp_5_1_2
        //val Do_temp2_5_2_2 = limi_translation_5_3 * Bo_temp_5_2_1 - limi_translation_5_1 * Bo_temp_5_2_3 + Do_temp_5_2_2
        //val Do_temp2_5_3_2 = limi_translation_5_1 * Bo_temp_5_2_2 - limi_translation_5_2 * Bo_temp_5_2_1 + Do_temp_5_3_2
        //val Do_temp2_5_1_3 = limi_translation_5_2 * Bo_temp_5_3_3 - limi_translation_5_3 * Bo_temp_5_3_2 + Do_temp_5_1_3
        //val Do_temp2_5_2_3 = limi_translation_5_3 * Bo_temp_5_3_1 - limi_translation_5_1 * Bo_temp_5_3_3 + Do_temp_5_2_3
        //val Do_temp2_5_3_3 = limi_translation_5_1 * Bo_temp_5_3_2 - limi_translation_5_2 * Bo_temp_5_3_1 + Do_temp_5_3_3

        val Do_temp2_5_1_1 = 0.384 * Bo_temp_5_1_3 + Do_temp_5_1_1
        val Do_temp2_5_1_2 = 0.083 * Bo_temp_5_1_3 + Do_temp_5_1_2
        val Do_temp2_5_1_3 = -0.083 * Bo_temp_5_1_2 - 0.384 * Bo_temp_5_1_1 + Do_temp_5_1_3
        val Do_temp2_5_2_1 = 0.384 * Bo_temp_5_2_3 + Do_temp_5_2_1
        val Do_temp2_5_2_2 = 0.083 * Bo_temp_5_2_3 + Do_temp_5_2_2
        val Do_temp2_5_2_3 = -0.083 * Bo_temp_5_2_2 - 0.384 * Bo_temp_5_2_1 + Do_temp_5_2_3
        val Do_temp2_5_3_1 = 0.384 * Bo_temp_5_3_3 + Do_temp_5_3_1
        val Do_temp2_5_3_2 = 0.083 * Bo_temp_5_3_3 + Do_temp_5_3_2
        val Do_temp2_5_3_3 = -0.083 * Bo_temp_5_3_2 - 0.384 * Bo_temp_5_3_1 + Do_temp_5_3_3

        // Now, we'll have 
        // Co.col(1) = limi_translation.cross(Ao.col(1)) + Bo.col(1)
        // Co.col(2) = limi_translation.cross(Ao.col(2)) + Bo.col(2)
        // Co.col(3) = limi_translation.cross(Ao.col(3)) + Bo.col(3)
        //val Co_5_1_1 = limi_translation_5_2 * Ao_5_1_3 - limi_translation_5_3 * Ao_5_1_2 + Bo_temp_5_1_1
        //val Co_5_1_2 = limi_translation_5_3 * Ao_5_1_1 - limi_translation_5_1 * Ao_5_1_3 + Bo_temp_5_1_2
        //val Co_5_1_3 = limi_translation_5_1 * Ao_5_1_2 - limi_translation_5_2 * Ao_5_1_1 + Bo_temp_5_1_3
        //val Co_5_2_1 = limi_translation_5_2 * Ao_5_2_3 - limi_translation_5_3 * Ao_5_2_2 + Bo_temp_5_2_1
        //val Co_5_2_2 = limi_translation_5_3 * Ao_5_2_1 - limi_translation_5_1 * Ao_5_2_3 + Bo_temp_5_2_2
        //val Co_5_2_3 = limi_translation_5_1 * Ao_5_2_2 - limi_translation_5_2 * Ao_5_2_1 + Bo_temp_5_2_3
        //val Co_5_3_1 = limi_translation_5_2 * Ao_5_3_3 - limi_translation_5_3 * Ao_5_3_2 + Bo_temp_5_3_1
        //val Co_5_3_2 = limi_translation_5_3 * Ao_5_3_1 - limi_translation_5_1 * Ao_5_3_3 + Bo_temp_5_3_2
        //val Co_5_3_3 = limi_translation_5_1 * Ao_5_3_2 - limi_translation_5_2 * Ao_5_3_1 + Bo_temp_5_3_3

        val Co_5_1_1 = 0.384 * Ao_5_1_3 + Bo_temp_5_1_1
        val Co_5_2_1 = 0.083 * Ao_5_1_3 + Bo_temp_5_1_2
        val Co_5_3_1 = -0.083 * Ao_5_1_2 - 0.384 * Ao_5_1_1 + Bo_temp_5_1_3
        val Co_5_1_2 = 0.384 * Ao_5_2_3 + Bo_temp_5_2_1
        val Co_5_2_2 = 0.083 * Ao_5_2_3 + Bo_temp_5_2_2
        val Co_5_3_2 = -0.083 * Ao_5_2_2 - 0.384 * Ao_5_2_1 + Bo_temp_5_2_3
        val Co_5_1_3 = 0.384 * Ao_5_3_3 + Bo_temp_5_3_1
        val Co_5_2_3 = 0.083 * Ao_5_3_3 + Bo_temp_5_3_2
        val Co_5_3_3 = -0.083 * Ao_5_3_2 - 0.384 * Ao_5_3_1 + Bo_temp_5_3_3

        // Now, we'll have Bo = Co.transpose()
        //val Bo_5_1_1 = Co_5_1_1
        //val Bo_5_1_2 = Co_5_2_1
        //val Bo_5_1_3 = Co_5_3_1
        //val Bo_5_2_1 = Co_5_1_2
        //val Bo_5_2_2 = Co_5_2_2
        //val Bo_5_2_3 = Co_5_3_2
        //val Bo_5_3_1 = Co_5_1_3
        //val Bo_5_3_2 = Co_5_2_3
        //val Bo_5_3_3 = Co_5_3_3

        val Bo_5_1_1 = Co_5_1_1
        val Bo_5_1_2 = Co_5_2_1
        val Bo_5_1_3 = Co_5_3_1
        val Bo_5_2_1 = Co_5_1_2
        val Bo_5_2_2 = Co_5_2_2
        val Bo_5_2_3 = Co_5_3_2
        val Bo_5_3_1 = Co_5_1_3
        val Bo_5_3_2 = Co_5_2_3
        val Bo_5_3_3 = Co_5_3_3

        // Now, Do = Do_temp2 + limi_translation.cross(Bo)
        //val Do_5_1_1 = -limi_translation_5_2 * Bo_5_1_3 + limi_translation_5_3 * Bo_5_1_2 + Do_temp2_5_1_1
        //val Do_5_1_2 = -limi_translation_5_3 * Bo_5_1_1 + limi_translation_5_1 * Bo_5_1_3 + Do_temp2_5_1_2
        //val Do_5_1_3 = -limi_translation_5_1 * Bo_5_1_2 + limi_translation_5_2 * Bo_5_1_1 + Do_temp2_5_1_3
        //val Do_5_2_1 = -limi_translation_5_2 * Bo_5_2_3 + limi_translation_5_3 * Bo_5_2_2 + Do_temp2_5_2_1
        //val Do_5_2_2 = -limi_translation_5_3 * Bo_5_2_1 + limi_translation_5_1 * Bo_5_2_3 + Do_temp2_5_2_2
        //val Do_5_2_3 = -limi_translation_5_1 * Bo_5_2_2 + limi_translation_5_2 * Bo_5_2_1 + Do_temp2_5_2_3
        //val Do_5_3_1 = -limi_translation_5_2 * Bo_5_3_3 + limi_translation_5_3 * Bo_5_3_2 + Do_temp2_5_3_1
        //val Do_5_3_2 = -limi_translation_5_3 * Bo_5_3_1 + limi_translation_5_1 * Bo_5_3_3 + Do_temp2_5_3_2
        //val Do_5_3_3 = -limi_translation_5_1 * Bo_5_3_2 + limi_translation_5_2 * Bo_5_3_1 + Do_temp2_5_3_3

        val Do_5_1_1 = -0.384 * Bo_5_1_3                    + Do_temp2_5_1_1
        val Do_5_2_1 = -0.083 * Bo_5_1_3                    + Do_temp2_5_1_2
        val Do_5_3_1 = 0.083 * Bo_5_1_2 + 0.384 * Bo_5_1_1  + Do_temp2_5_1_3
        val Do_5_1_2 = -0.384 * Bo_5_2_3                    + Do_temp2_5_2_1
        val Do_5_2_2 = -0.083 * Bo_5_2_3                    + Do_temp2_5_2_2
        val Do_5_3_2 = 0.083 * Bo_5_2_2 + 0.384 * Bo_5_2_1  + Do_temp2_5_2_3
        val Do_5_1_3 = -0.384 * Bo_5_3_3                    + Do_temp2_5_3_1
        val Do_5_2_3 = -0.083 * Bo_5_3_3                    + Do_temp2_5_3_2
        val Do_5_3_3 = 0.083 * Bo_5_3_2 + 0.384 * Bo_5_3_1  + Do_temp2_5_3_3

        //val Do_5_1_1 = 0.384 * Bo_5_1_3 + Do_temp2_5_1_1
        //val Do_5_1_2 = 0.083 * Bo_5_1_3 + Do_temp2_5_1_2
        //val Do_5_1_3 = -0.083 * Bo_5_1_2 - 0.384 * Bo_5_1_1 + Do_temp2_5_1_3
        //val Do_5_2_1 = 0.384 * Bo_5_2_3 + Do_temp2_5_2_1
        //val Do_5_2_2 = 0.083 * Bo_5_2_3 + Do_temp2_5_2_2
        //val Do_5_2_3 = -0.083 * Bo_5_2_2 - 0.384 * Bo_5_2_1 + Do_temp2_5_2_3
        //val Do_5_3_1 = 0.384 * Bo_5_3_3 + Do_temp2_5_3_1
        //val Do_5_3_2 = 0.083 * Bo_5_3_3 + Do_temp2_5_3_2
        //val Do_5_3_3 = -0.083 * Bo_5_3_2 - 0.384 * Bo_5_3_1 + Do_temp2_5_3_3

        // Finally, add this to Yaba[4]
        //Yaba[4] += acton(limi[5], Ia)
        //val Ia_4_1_1 = Ao_5_1_1 + 3.5879
        //val Ia_4_1_2 = Ao_5_1_2 + 0.0
        //val Ia_4_1_3 = Ao_5_1_3 + 0.0
        //val Ia_4_2_1 = Ao_5_2_1 + 0.0
        //val Ia_4_2_2 = Ao_5_2_2 + 3.5879
        //val Ia_4_2_3 = Ao_5_2_3 + 0.0
        //val Ia_4_3_1 = Ao_5_3_1 + 0.0
        //val Ia_4_3_2 = Ao_5_3_2 + 0.0
        //val Ia_4_3_3 = Ao_5_3_3 + 3.5879
        //val Ia_4_4_1 = Co_5_1_1 + 0.0
        //val Ia_4_4_2 = Co_5_1_2 - 0.0985021
        //val Ia_4_4_3 = Co_5_1_3 + 0.374644
        //val Ia_4_5_1 = Co_5_2_1 + 0.0985021
        //val Ia_4_5_2 = Co_5_2_2 + 0.0
        //val Ia_4_5_3 = Co_5_2_3 + 0.190768
        //val Ia_4_6_1 = Co_5_3_1 - 0.374644
        //val Ia_4_6_2 = Co_5_3_2 - 0.190768
        //val Ia_4_6_3 = Co_5_3_3 + 0.0
        //val Ia_4_1_4 = Bo_5_1_1 + 0.0
        //val Ia_4_1_5 = Bo_5_1_2 + 0.0985021
        //val Ia_4_1_6 = Bo_5_1_3 - 0.374644
        //val Ia_4_2_4 = Bo_5_2_1 - 0.0985021
        //val Ia_4_2_5 = Bo_5_2_2 + 0.0
        //val Ia_4_2_6 = Bo_5_2_3 - 0.190768
        //val Ia_4_3_4 = Bo_5_3_1 + 0.374644
        //val Ia_4_3_5 = Bo_5_3_2 + 0.190768
        //val Ia_4_3_6 = Bo_5_3_3 + 0.0
        //val Ia_4_4_4 = Do_5_1_1 + 0.0676773
        //val Ia_4_4_5 = Do_5_1_2 + 0.0277158
        //val Ia_4_4_6 = Do_5_1_3 + 0.00390536
        //val Ia_4_5_4 = Do_5_2_1 + 0.0277158
        //val Ia_4_5_5 = Do_5_2_2 + 0.0323994
        //val Ia_4_5_6 = Do_5_2_3 - 0.00164449
        //val Ia_4_6_4 = Do_5_3_1 + 0.00390536
        //val Ia_4_6_5 = Do_5_3_2 - 0.00164449
        //val Ia_4_6_6 = Do_5_3_3 + 0.0775861

        val Ia_4_1_1 = Ao_5_1_1 + 3.5879
        val Ia_4_1_2 = Ao_5_1_2 + 0.0
        val Ia_4_1_3 = Ao_5_1_3 + 0.0
        val Ia_4_2_1 = Ao_5_2_1 + 0.0
        val Ia_4_2_2 = Ao_5_2_2 + 3.5879
        val Ia_4_2_3 = Ao_5_2_3 + 0.0
        val Ia_4_3_1 = Ao_5_3_1 + 0.0
        val Ia_4_3_2 = Ao_5_3_2 + 0.0
        val Ia_4_3_3 = Ao_5_3_3 + 3.5879
        val Ia_4_4_1 = Co_5_1_1 + 0.0
        val Ia_4_4_2 = Co_5_1_2 - 0.0985021
        val Ia_4_4_3 = Co_5_1_3 + 0.374644
        val Ia_4_5_1 = Co_5_2_1 + 0.0985021
        val Ia_4_5_2 = Co_5_2_2 + 0.0
        val Ia_4_5_3 = Co_5_2_3 + 0.190768
        val Ia_4_6_1 = Co_5_3_1 - 0.374644
        val Ia_4_6_2 = Co_5_3_2 - 0.190768
        val Ia_4_6_3 = Co_5_3_3 + 0.0
        val Ia_4_1_4 = Bo_5_1_1 + 0.0
        val Ia_4_1_5 = Bo_5_1_2 + 0.0985021
        val Ia_4_1_6 = Bo_5_1_3 - 0.374644
        val Ia_4_2_4 = Bo_5_2_1 - 0.0985021
        val Ia_4_2_5 = Bo_5_2_2 + 0.0
        val Ia_4_2_6 = Bo_5_2_3 - 0.190768
        val Ia_4_3_4 = Bo_5_3_1 + 0.374644
        val Ia_4_3_5 = Bo_5_3_2 + 0.190768
        val Ia_4_3_6 = Bo_5_3_3 + 0.0
        val Ia_4_4_4 = Do_5_1_1 + 0.0676773
        val Ia_4_4_5 = Do_5_1_2 + 0.0277158
        val Ia_4_4_6 = Do_5_1_3 + 0.00390536
        val Ia_4_5_4 = Do_5_2_1 + 0.0277158
        val Ia_4_5_5 = Do_5_2_2 + 0.0323994
        val Ia_4_5_6 = Do_5_2_3 - 0.00164449
        val Ia_4_6_4 = Do_5_3_1 + 0.00390536
        val Ia_4_6_5 = Do_5_3_2 - 0.00164449
        val Ia_4_6_6 = Do_5_3_3 + 0.0775861

        // JOINT 4
        val U_4_1 = Ia_4_1_6
        val U_4_2 = Ia_4_2_6
        val U_4_3 = Ia_4_3_6
        val U_4_4 = Ia_4_4_6
        val U_4_5 = Ia_4_5_6
        val U_4_6 = Ia_4_6_6    

        val Dinv_4_1 = 1.0 / U_4_6

        Dinv_4_1

    } //ensuring(res => res +/- 1e-10)



  

}


