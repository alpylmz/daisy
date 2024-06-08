import daisy.lang._
import Real._
import javax.security.sasl.RealmCallback




object Minv {
  
    /*
    def alphaSkewSquareOptimized(m: Real, x: Real, y: Real, z: Real) = {
        require(m > 0 && m < 5.0 && x > -0.12 && y > -0.12 && z > -0.12 && x < 0.18 && y < 0.18 && z < 0.18)
            // alphaskewsquare returns a skew-symmetric matrix
            val z_z = z * z
            val y_y = y * y
            val x_x = x * x
            val m_1_1 = -m * (y_y + z_z)
            val m_1_2 = m * x * y
            val m_1_3 = m * x * z
            val m_2_1 = m_1_2
            val m_2_2 = -m * (x_x + z_z)
            val m_2_3 = m * y * z
            val m_3_1 = m_1_3
            val m_3_2 = m_2_3
            val m_3_3 = -m * (x_x + y_y)
            m_1_1 + m_1_2 + m_1_3 + m_2_1 + m_2_2 + m_2_3 + m_3_1 + m_3_2 + m_3_3
            // res cannot be a vector or a matrix, so I am just putting one value here as a placeholder
    } ensuring(res => res +/- 1e-2)
    */
    /*
    def alphaSkew(m: Real, x: Real, y: Real, z: Real) = {
        require(m > 0 && m < 5.0 && x > -0.12 && y > -0.12 && z > -0.12 && x < 0.18 && y < 0.18 && z < 0.18)
            // alphaskew returns a skew-symmetric matrix
            //val m_1_1 = 0.0
            val m_1_2 = -m * z
            val m_1_3 = m * y
            val m_2_1 = -m_1_2
            //val m_2_2 = 0.0
            val m_2_3 = -m * x
            val m_3_1 = -m_1_3
            val m_3_2 = -m_2_3
            //val m_3_3 = 0.0
            m_1_2 + m_1_3 + m_2_1 + m_2_3 + m_3_1 + m_3_2 // + m_1_1 + m_2_2 + m_3_3
            // res cannot be a vector or a matrix, so I am just putting one value here as a placeholder
    } ensuring(res => res +/- 1e-2)
    */

    // this function is model.inertias[i].matrix()
    // inertia function
    // the first 3x3 block is the mass times the identity matrix
    // the second 3x3 block is the skew-symmetric matrix alphaSkew
    // the third 3x3 block is -alphaSkew
    // the fourth 3x3 block is inertia - alphaSkewSquare
    /*
    def first_pass_inertia(
        m: Real,
        x_lever: Real,
        y_lever: Real,
        z_lever: Real,
        joint_inertia_1: Real,
        joint_inertia_2: Real,
        joint_inertia_3: Real,
        joint_inertia_4: Real,
        joint_inertia_5: Real,
        joint_inertia_6: Real
    ) = {
        require(
            m > 0 && m < 5.0 && 
            x_lever > -0.12 && y_lever > -0.12 && z_lever > -0.12 && 
            x_lever < 0.18 && y_lever < 0.18 && z_lever < 0.18 &&
            joint_inertia_1 > -0.03 && joint_inertia_2 > -0.03 && joint_inertia_3 > -0.03 && joint_inertia_4 > -0.03 && joint_inertia_5 > -0.03 && joint_inertia_6 > -0.03 &&
            joint_inertia_1 < 1.2 && joint_inertia_2 < 1.2 && joint_inertia_3 < 1.2 && joint_inertia_4 < 1.2 && joint_inertia_5 < 1.2 && joint_inertia_6 < 1.2
        )
        // fi is first_block
        val fi_1_1 = m
        val fi_2_2 = m
        val fi_3_3 = m
        // rest is 0.0
        
        // se is second_block
        val se_1_2 = m * z_lever
        val se_1_3 = -m * y_lever
        val se_2_1 = -se_1_2
        val se_2_3 = m * x_lever
        val se_3_1 = -se_1_3
        val se_3_2 = -se_2_3
        
        // th is third_block
        val th_1_2 = -se_1_2
        val th_1_3 = -se_1_3
        val th_2_1 = -se_2_1
        val th_2_3 = -se_2_3
        val th_3_1 = -se_3_1
        val th_3_2 = -se_3_2

        // fo is fourth_block
        val fo_1_1 = joint_inertia_1 + m * (y_lever * y_lever + z_lever * z_lever)
        val fo_1_2 = joint_inertia_2 - m * x_lever * y_lever
        val fo_1_3 = joint_inertia_4 - m * x_lever * z_lever // problem here
        val fo_2_1 = fo_1_2
        val fo_2_2 = joint_inertia_3 + m * (x_lever * x_lever + z_lever * z_lever)
        val fo_2_3 = joint_inertia_5 - m * y_lever * z_lever
        val fo_3_1 = fo_1_3
        val fo_3_2 = fo_2_3
        val fo_3_3 = joint_inertia_6 + m * (x_lever * x_lever + y_lever * y_lever)

        // res cannot be a vector or a matrix, so I am just putting one value here as a placeholder
        fi_1_1 + fi_2_2 + fi_3_3 + se_1_2 + se_1_3 + se_2_1 + se_2_3 + se_3_1 + se_3_2 + th_1_2 + th_1_3 + th_2_1 + th_2_3 + th_3_1 + th_3_2 + fo_1_1 + fo_1_2 + fo_1_3 + fo_2_1 + fo_2_2 + fo_2_3 + fo_3_1 + fo_3_2 + fo_3_3
    } ensuring(res => res +/- 1e-2)
    */

    /*
    def calculate_oMi_firstPass(
        model_joint_p_rotation_1_1: Real, model_joint_p_rotation_1_2: Real, model_joint_p_rotation_1_3: Real,
        model_joint_p_rotation_2_1: Real, model_joint_p_rotation_2_2: Real, model_joint_p_rotation_2_3: Real,
        model_joint_p_rotation_3_1: Real, model_joint_p_rotation_3_2: Real, model_joint_p_rotation_3_3: Real,
        model_joint_p_translation_1: Real, model_joint_p_translation_2: Real, model_joint_p_translation_3: Real,
        qpos: Real
    ) = {
        require(
            model_joint_p_rotation_1_1 > -1.0 && model_joint_p_rotation_1_1 < 1.0 &&
            model_joint_p_rotation_1_2 > -1.0 && model_joint_p_rotation_1_2 < 1.0 &&
            model_joint_p_rotation_1_3 > -1.0 && model_joint_p_rotation_1_3 < 1.0 &&
            model_joint_p_rotation_2_1 > -1.0 && model_joint_p_rotation_2_1 < 1.0 &&
            model_joint_p_rotation_2_2 > -1.0 && model_joint_p_rotation_2_2 < 1.0 &&
            model_joint_p_rotation_2_3 > -1.0 && model_joint_p_rotation_2_3 < 1.0 &&
            model_joint_p_rotation_3_1 > -1.0 && model_joint_p_rotation_3_1 < 1.0 &&
            model_joint_p_rotation_3_2 > -1.0 && model_joint_p_rotation_3_2 < 1.0 &&
            model_joint_p_rotation_3_3 > -1.0 && model_joint_p_rotation_3_3 < 1.0 &&
            model_joint_p_translation_1 > -1.0 && model_joint_p_translation_1 < 1.0 &&
            model_joint_p_translation_2 > -1.0 && model_joint_p_translation_2 < 1.0 &&
            model_joint_p_translation_3 > -1.0 && model_joint_p_translation_3 < 1.0 &&
            qpos > -4.9 && qpos < 3.0
        )
        val sin_qpos = sin(qpos)
        val cos_qpos = cos(qpos)

        val rotation_matrix_1_1 = cos_qpos
        val rotation_matrix_1_2 = sin_qpos
        //val rotation_matrix_1_3 = 0.0
        val rotation_matrix_2_1 = -sin_qpos
        val rotation_matrix_2_2 = cos_qpos
        //val rotation_matrix_2_3 = 0.0
        //val rotation_matrix_3_1 = 0.0
        //val rotation_matrix_3_2 = 0.0
        val rotation_matrix_3_3 = 1.0

        // limi.rotation is joint_placement_rotation * rotation_matrix
        val limi_rotation_1_1 = model_joint_p_rotation_1_1 * rotation_matrix_1_1 + model_joint_p_rotation_1_2 * rotation_matrix_2_1
        val limi_rotation_1_2 = model_joint_p_rotation_1_1 * rotation_matrix_1_2 + model_joint_p_rotation_1_2 * rotation_matrix_2_2
        val limi_rotation_1_3 = model_joint_p_rotation_1_3 * rotation_matrix_3_3
        val limi_rotation_2_1 = model_joint_p_rotation_2_1 * rotation_matrix_1_1 + model_joint_p_rotation_2_2 * rotation_matrix_2_1
        val limi_rotation_2_2 = model_joint_p_rotation_2_1 * rotation_matrix_1_2 + model_joint_p_rotation_2_2 * rotation_matrix_2_2
        val limi_rotation_2_3 = model_joint_p_rotation_2_3 * rotation_matrix_3_3
        val limi_rotation_3_1 = model_joint_p_rotation_3_1 * rotation_matrix_1_1 + model_joint_p_rotation_3_2 * rotation_matrix_2_1
        val limi_rotation_3_2 = model_joint_p_rotation_3_1 * rotation_matrix_1_2 + model_joint_p_rotation_3_2 * rotation_matrix_2_2
        val limi_rotation_3_3 = model_joint_p_rotation_3_3 * rotation_matrix_3_3

        // limi.translation is model_joint_p
        val limi_translation_1 = model_joint_p_translation_1
        val limi_translation_2 = model_joint_p_translation_2
        val limi_translation_3 = model_joint_p_translation_3

        // test until now
        //limi_rotation_1_1 + limi_rotation_1_2 + limi_rotation_1_3 + limi_rotation_2_1 + limi_rotation_2_2 + limi_rotation_2_3 + limi_rotation_3_1 + limi_rotation_3_2 + limi_rotation_3_3 + limi_translation_1 + limi_translation_2 + limi_translation_3
        
        // if parent > 0, then oMi is omi[parent] * limi
        // I'll abandon this here since I don't have the parent variable
    }
    */

    def firstPass(
        // model_joint_p_rotation_<joint_id>_<row>_<column>
        model_joint_p_rotation_1_1_1: Real, model_joint_p_rotation_1_1_2: Real, model_joint_p_rotation_1_1_3: Real,
        model_joint_p_rotation_1_2_1: Real, model_joint_p_rotation_1_2_2: Real, model_joint_p_rotation_1_2_3: Real,
        model_joint_p_rotation_1_3_1: Real, model_joint_p_rotation_1_3_2: Real, model_joint_p_rotation_1_3_3: Real,
        model_joint_p_translation_1_1: Real, model_joint_p_translation_1_2: Real, model_joint_p_translation_1_3: Real,

        model_joint_p_rotation_2_1_1: Real, model_joint_p_rotation_2_1_2: Real, model_joint_p_rotation_2_1_3: Real,
        model_joint_p_rotation_2_2_1: Real, model_joint_p_rotation_2_2_2: Real, model_joint_p_rotation_2_2_3: Real,
        model_joint_p_rotation_2_3_1: Real, model_joint_p_rotation_2_3_2: Real, model_joint_p_rotation_2_3_3: Real,
        model_joint_p_translation_2_1: Real, model_joint_p_translation_2_2: Real, model_joint_p_translation_2_3: Real,

        model_joint_p_rotation_3_1_1: Real, model_joint_p_rotation_3_1_2: Real, model_joint_p_rotation_3_1_3: Real,
        model_joint_p_rotation_3_2_1: Real, model_joint_p_rotation_3_2_2: Real, model_joint_p_rotation_3_2_3: Real,
        model_joint_p_rotation_3_3_1: Real, model_joint_p_rotation_3_3_2: Real, model_joint_p_rotation_3_3_3: Real,
        model_joint_p_translation_3_1: Real, model_joint_p_translation_3_2: Real, model_joint_p_translation_3_3: Real,

        qpos1: Real,
        qpos2: Real,
        qpos3: Real
    ) = {
        require(
            model_joint_p_rotation_1_1_1 > -1.0 && model_joint_p_rotation_1_1_1 < 1.0 && model_joint_p_rotation_1_1_2 > -1.0 && model_joint_p_rotation_1_1_2 < 1.0 && model_joint_p_rotation_1_1_3 > -1.0 && model_joint_p_rotation_1_1_3 < 1.0 &&
            model_joint_p_rotation_1_2_1 > -1.0 && model_joint_p_rotation_1_2_1 < 1.0 && model_joint_p_rotation_1_2_2 > -1.0 && model_joint_p_rotation_1_2_2 < 1.0 && model_joint_p_rotation_1_2_3 > -1.0 && model_joint_p_rotation_1_2_3 < 1.0 &&
            model_joint_p_rotation_1_3_1 > -1.0 && model_joint_p_rotation_1_3_1 < 1.0 && model_joint_p_rotation_1_3_2 > -1.0 && model_joint_p_rotation_1_3_2 < 1.0 && model_joint_p_rotation_1_3_3 > -1.0 && model_joint_p_rotation_1_3_3 < 1.0 &&

            model_joint_p_translation_1_1 > -1.0 && model_joint_p_translation_1_1 < 1.0 && 
            model_joint_p_translation_1_2 > -1.0 && model_joint_p_translation_1_2 < 1.0 && 
            model_joint_p_translation_1_3 > -1.0 && model_joint_p_translation_1_3 < 1.0 &&

            model_joint_p_rotation_2_1_1 > -1.0 && model_joint_p_rotation_2_1_1 < 1.0 && model_joint_p_rotation_2_1_2 > -1.0 && model_joint_p_rotation_2_1_2 < 1.0 && model_joint_p_rotation_2_1_3 > -1.0 && model_joint_p_rotation_2_1_3 < 1.0 &&
            model_joint_p_rotation_2_2_1 > -1.0 && model_joint_p_rotation_2_2_1 < 1.0 && model_joint_p_rotation_2_2_2 > -1.0 && model_joint_p_rotation_2_2_2 < 1.0 && model_joint_p_rotation_2_2_3 > -1.0 && model_joint_p_rotation_2_2_3 < 1.0 &&
            model_joint_p_rotation_2_3_1 > -1.0 && model_joint_p_rotation_2_3_1 < 1.0 && model_joint_p_rotation_2_3_2 > -1.0 && model_joint_p_rotation_2_3_2 < 1.0 && model_joint_p_rotation_2_3_3 > -1.0 && model_joint_p_rotation_2_3_3 < 1.0 &&

            model_joint_p_translation_2_1 > -1.0 && model_joint_p_translation_2_1 < 1.0 &&
            model_joint_p_translation_2_2 > -1.0 && model_joint_p_translation_2_2 < 1.0 &&
            model_joint_p_translation_2_3 > -1.0 && model_joint_p_translation_2_3 < 1.0 &&

            model_joint_p_rotation_3_1_1 > -1.0 && model_joint_p_rotation_3_1_1 < 1.0 && model_joint_p_rotation_3_1_2 > -1.0 && model_joint_p_rotation_3_1_2 < 1.0 && model_joint_p_rotation_3_1_3 > -1.0 && model_joint_p_rotation_3_1_3 < 1.0 &&
            model_joint_p_rotation_3_2_1 > -1.0 && model_joint_p_rotation_3_2_1 < 1.0 && model_joint_p_rotation_3_2_2 > -1.0 && model_joint_p_rotation_3_2_2 < 1.0 && model_joint_p_rotation_3_2_3 > -1.0 && model_joint_p_rotation_3_2_3 < 1.0 &&
            model_joint_p_rotation_3_3_1 > -1.0 && model_joint_p_rotation_3_3_1 < 1.0 && model_joint_p_rotation_3_3_2 > -1.0 && model_joint_p_rotation_3_3_2 < 1.0 && model_joint_p_rotation_3_3_3 > -1.0 && model_joint_p_rotation_3_3_3 < 1.0 &&

            model_joint_p_translation_3_1 > -1.0 && model_joint_p_translation_3_1 < 1.0 &&
            model_joint_p_translation_3_2 > -1.0 && model_joint_p_translation_3_2 < 1.0 &&
            model_joint_p_translation_3_3 > -1.0 && model_joint_p_translation_3_3 < 1.0 &&

            qpos1 > -4.9 && qpos1 < 3.0 &&
            qpos2 > -4.9 && qpos2 < 3.0 &&
            qpos3 > -4.9 && qpos3 < 3.0

        )

        // JOINT 1
        val sin_qpos1 = sin(qpos1)
        val cos_qpos1 = cos(qpos1)

        val rotation_matrix_1_1_1 = cos_qpos1
        val rotation_matrix_1_1_2 = -sin_qpos1
        val rotation_matrix_1_2_1 = sin_qpos1
        val rotation_matrix_1_2_2 = cos_qpos1
        // val rotation_matrix_1_3_3 = 1.0 for some reason daisy does not like this line

        val limi_rotation_1_1_1 = model_joint_p_rotation_1_1_1 * rotation_matrix_1_1_1 + model_joint_p_rotation_1_1_2 * rotation_matrix_1_2_1
        val limi_rotation_1_1_2 = model_joint_p_rotation_1_1_1 * rotation_matrix_1_1_2 + model_joint_p_rotation_1_1_2 * rotation_matrix_1_2_2
        val limi_rotation_1_1_3 = model_joint_p_rotation_1_1_3// * rotation_matrix_1_3_3
        val limi_rotation_1_2_1 = model_joint_p_rotation_1_2_1 * rotation_matrix_1_1_1 + model_joint_p_rotation_1_2_2 * rotation_matrix_1_2_1
        val limi_rotation_1_2_2 = model_joint_p_rotation_1_2_1 * rotation_matrix_1_1_2 + model_joint_p_rotation_1_2_2 * rotation_matrix_1_2_2
        val limi_rotation_1_2_3 = model_joint_p_rotation_1_2_3// * rotation_matrix_1_3_3
        val limi_rotation_1_3_1 = model_joint_p_rotation_1_3_1 * rotation_matrix_1_1_1 + model_joint_p_rotation_1_3_2 * rotation_matrix_1_2_1
        val limi_rotation_1_3_2 = model_joint_p_rotation_1_3_1 * rotation_matrix_1_1_2 + model_joint_p_rotation_1_3_2 * rotation_matrix_1_2_2
        val limi_rotation_1_3_3 = model_joint_p_rotation_1_3_3// * rotation_matrix_1_3_3

        val limi_translation_1_1 = model_joint_p_translation_1_1
        val limi_translation_1_2 = model_joint_p_translation_1_2
        val limi_translation_1_3 = model_joint_p_translation_1_3

        //limi_rotation_1_1_1 + limi_rotation_1_1_2 + limi_rotation_1_1_3 + limi_rotation_1_2_1 + limi_rotation_1_2_2 + limi_rotation_1_2_3 + limi_rotation_1_3_1 + limi_rotation_1_3_2 + limi_rotation_1_3_3 + limi_translation_1_1 + limi_translation_1_2 + limi_translation_1_3
        
        // parent = 0, so oMi is limi
        val oMi_rotation_1_1_1 = limi_rotation_1_1_1
        val oMi_rotation_1_1_2 = limi_rotation_1_1_2
        val oMi_rotation_1_1_3 = limi_rotation_1_1_3
        val oMi_rotation_1_2_1 = limi_rotation_1_2_1
        val oMi_rotation_1_2_2 = limi_rotation_1_2_2
        val oMi_rotation_1_2_3 = limi_rotation_1_2_3
        val oMi_rotation_1_3_1 = limi_rotation_1_3_1
        val oMi_rotation_1_3_2 = limi_rotation_1_3_2
        val oMi_rotation_1_3_3 = limi_rotation_1_3_3

        val oMi_translation_1_1 = limi_translation_1_1
        val oMi_translation_1_2 = limi_translation_1_2
        val oMi_translation_1_3 = limi_translation_1_3

        //oMi_rotation_1_1_1 + oMi_rotation_1_1_2 + oMi_rotation_1_1_3 + oMi_rotation_1_2_1 + oMi_rotation_1_2_2 + oMi_rotation_1_2_3 + oMi_rotation_1_3_1 + oMi_rotation_1_3_2 + oMi_rotation_1_3_3 + oMi_translation_1_1 + oMi_translation_1_2 + oMi_translation_1_3

        // JOINT 2
        val sin_qpos2 = sin(qpos2)
        val cos_qpos2 = cos(qpos2)

        val rotation_matrix_2_1_1 = cos_qpos2
        val rotation_matrix_2_1_2 = -sin_qpos2
        val rotation_matrix_2_2_1 = sin_qpos2
        val rotation_matrix_2_2_2 = cos_qpos2
        // val rotation_matrix_2_3_3 = 1.0 for some reason daisy does not like this line

        val limi_rotation_2_1_1 = model_joint_p_rotation_2_1_1 * rotation_matrix_2_1_1 + model_joint_p_rotation_2_1_2 * rotation_matrix_2_2_1
        val limi_rotation_2_1_2 = model_joint_p_rotation_2_1_1 * rotation_matrix_2_1_2 + model_joint_p_rotation_2_1_2 * rotation_matrix_2_2_2
        val limi_rotation_2_1_3 = model_joint_p_rotation_2_1_3// * rotation_matrix_2_3_3
        val limi_rotation_2_2_1 = model_joint_p_rotation_2_2_1 * rotation_matrix_2_1_1 + model_joint_p_rotation_2_2_2 * rotation_matrix_2_2_1
        val limi_rotation_2_2_2 = model_joint_p_rotation_2_2_1 * rotation_matrix_2_1_2 + model_joint_p_rotation_2_2_2 * rotation_matrix_2_2_2
        val limi_rotation_2_2_3 = model_joint_p_rotation_2_2_3// * rotation_matrix_2_3_3
        val limi_rotation_2_3_1 = model_joint_p_rotation_2_3_1 * rotation_matrix_2_1_1 + model_joint_p_rotation_2_3_2 * rotation_matrix_2_2_1
        val limi_rotation_2_3_2 = model_joint_p_rotation_2_3_1 * rotation_matrix_2_1_2 + model_joint_p_rotation_2_3_2 * rotation_matrix_2_2_2
        val limi_rotation_2_3_3 = model_joint_p_rotation_2_3_3// * rotation_matrix_2_3_3

        val limi_translation_2_1 = model_joint_p_translation_2_1
        val limi_translation_2_2 = model_joint_p_translation_2_2
        val limi_translation_2_3 = model_joint_p_translation_2_3

        // parent is 1, so oMi is oMi[1] * limi
        val oMi_rotation_2_1_1 = oMi_rotation_1_1_1 * limi_rotation_2_1_1 + oMi_rotation_1_1_2 * limi_rotation_2_2_1 + oMi_rotation_1_1_3 * limi_rotation_2_3_1
        val oMi_rotation_2_1_2 = oMi_rotation_1_1_1 * limi_rotation_2_1_2 + oMi_rotation_1_1_2 * limi_rotation_2_2_2 + oMi_rotation_1_1_3 * limi_rotation_2_3_2
        val oMi_rotation_2_1_3 = oMi_rotation_1_1_1 * limi_rotation_2_1_3 + oMi_rotation_1_1_2 * limi_rotation_2_2_3 + oMi_rotation_1_1_3 * limi_rotation_2_3_3
        val oMi_rotation_2_2_1 = oMi_rotation_1_2_1 * limi_rotation_2_1_1 + oMi_rotation_1_2_2 * limi_rotation_2_2_1 + oMi_rotation_1_2_3 * limi_rotation_2_3_1
        val oMi_rotation_2_2_2 = oMi_rotation_1_2_1 * limi_rotation_2_1_2 + oMi_rotation_1_2_2 * limi_rotation_2_2_2 + oMi_rotation_1_2_3 * limi_rotation_2_3_2
        val oMi_rotation_2_2_3 = oMi_rotation_1_2_1 * limi_rotation_2_1_3 + oMi_rotation_1_2_2 * limi_rotation_2_2_3 + oMi_rotation_1_2_3 * limi_rotation_2_3_3
        val oMi_rotation_2_3_1 = oMi_rotation_1_3_1 * limi_rotation_2_1_1 + oMi_rotation_1_3_2 * limi_rotation_2_2_1 + oMi_rotation_1_3_3 * limi_rotation_2_3_1
        val oMi_rotation_2_3_2 = oMi_rotation_1_3_1 * limi_rotation_2_1_2 + oMi_rotation_1_3_2 * limi_rotation_2_2_2 + oMi_rotation_1_3_3 * limi_rotation_2_3_2
        val oMi_rotation_2_3_3 = oMi_rotation_1_3_1 * limi_rotation_2_1_3 + oMi_rotation_1_3_2 * limi_rotation_2_2_3 + oMi_rotation_1_3_3 * limi_rotation_2_3_3

        val oMi_translation_2_1 = oMi_rotation_1_1_1 * limi_translation_2_1 + oMi_rotation_1_2_1 * limi_translation_2_2 + oMi_rotation_1_3_1 * limi_translation_2_3 + oMi_translation_1_1
        val oMi_translation_2_2 = oMi_rotation_1_1_2 * limi_translation_2_1 + oMi_rotation_1_2_2 * limi_translation_2_2 + oMi_rotation_1_3_2 * limi_translation_2_3 + oMi_translation_1_2
        val oMi_translation_2_3 = oMi_rotation_1_1_3 * limi_translation_2_1 + oMi_rotation_1_2_3 * limi_translation_2_2 + oMi_rotation_1_3_3 * limi_translation_2_3 + oMi_translation_1_3

        // let's test until now
        //oMi_translation_2_1 + oMi_translation_2_2 + oMi_translation_2_3 + oMi_rotation_2_1_1 + oMi_rotation_2_1_2 + oMi_rotation_2_1_3 + oMi_rotation_2_2_1 + oMi_rotation_2_2_2 + oMi_rotation_2_2_3 + oMi_rotation_2_3_1 + oMi_rotation_2_3_2 + oMi_rotation_2_3_3

        // JOINT 3
        val sin_qpos3 = sin(qpos3)
        val cos_qpos3 = cos(qpos3)

        val rotation_matrix_3_1_1 = cos_qpos3
        val rotation_matrix_3_1_2 = -sin_qpos3
        val rotation_matrix_3_2_1 = sin_qpos3
        val rotation_matrix_3_2_2 = cos_qpos3
        // val rotation_matrix_3_3_3 = 1.0 for some reason daisy does not like this line

        val limi_rotation_3_1_1 = model_joint_p_rotation_3_1_1 * rotation_matrix_3_1_1 + model_joint_p_rotation_3_1_2 * rotation_matrix_3_2_1
        val limi_rotation_3_1_2 = model_joint_p_rotation_3_1_1 * rotation_matrix_3_1_2 + model_joint_p_rotation_3_1_2 * rotation_matrix_3_2_2
        val limi_rotation_3_1_3 = model_joint_p_rotation_3_1_3// * rotation_matrix_3_3_3
        val limi_rotation_3_2_1 = model_joint_p_rotation_3_2_1 * rotation_matrix_3_1_1 + model_joint_p_rotation_3_2_2 * rotation_matrix_3_2_1
        val limi_rotation_3_2_2 = model_joint_p_rotation_3_2_1 * rotation_matrix_3_1_2 + model_joint_p_rotation_3_2_2 * rotation_matrix_3_2_2
        val limi_rotation_3_2_3 = model_joint_p_rotation_3_2_3// * rotation_matrix_3_3_3
        val limi_rotation_3_3_1 = model_joint_p_rotation_3_3_1 * rotation_matrix_3_1_1 + model_joint_p_rotation_3_3_2 * rotation_matrix_3_2_1
        val limi_rotation_3_3_2 = model_joint_p_rotation_3_3_1 * rotation_matrix_3_1_2 + model_joint_p_rotation_3_3_2 * rotation_matrix_3_2_2
        val limi_rotation_3_3_3 = model_joint_p_rotation_3_3_3// * rotation_matrix_3_3_3

        val limi_translation_3_1 = model_joint_p_translation_3_1
        val limi_translation_3_2 = model_joint_p_translation_3_2
        val limi_translation_3_3 = model_joint_p_translation_3_3

        // parent is 2, so oMi is oMi[2] * limi
        val oMi_rotation_3_1_1 = oMi_rotation_2_1_1 * limi_rotation_3_1_1 + oMi_rotation_2_1_2 * limi_rotation_3_2_1 + oMi_rotation_2_1_3 * limi_rotation_3_3_1
        val oMi_rotation_3_1_2 = oMi_rotation_2_1_1 * limi_rotation_3_1_2 + oMi_rotation_2_1_2 * limi_rotation_3_2_2 + oMi_rotation_2_1_3 * limi_rotation_3_3_2
        val oMi_rotation_3_1_3 = oMi_rotation_2_1_1 * limi_rotation_3_1_3 + oMi_rotation_2_1_2 * limi_rotation_3_2_3 + oMi_rotation_2_1_3 * limi_rotation_3_3_3
        val oMi_rotation_3_2_1 = oMi_rotation_2_2_1 * limi_rotation_3_1_1 + oMi_rotation_2_2_2 * limi_rotation_3_2_1 + oMi_rotation_2_2_3 * limi_rotation_3_3_1
        val oMi_rotation_3_2_2 = oMi_rotation_2_2_1 * limi_rotation_3_1_2 + oMi_rotation_2_2_2 * limi_rotation_3_2_2 + oMi_rotation_2_2_3 * limi_rotation_3_3_2
        val oMi_rotation_3_2_3 = oMi_rotation_2_2_1 * limi_rotation_3_1_3 + oMi_rotation_2_2_2 * limi_rotation_3_2_3 + oMi_rotation_2_2_3 * limi_rotation_3_3_3
        val oMi_rotation_3_3_1 = oMi_rotation_2_3_1 * limi_rotation_3_1_1 + oMi_rotation_2_3_2 * limi_rotation_3_2_1 + oMi_rotation_2_3_3 * limi_rotation_3_3_1
        val oMi_rotation_3_3_2 = oMi_rotation_2_3_1 * limi_rotation_3_1_2 + oMi_rotation_2_3_2 * limi_rotation_3_2_2 + oMi_rotation_2_3_3 * limi_rotation_3_3_2
        val oMi_rotation_3_3_3 = oMi_rotation_2_3_1 * limi_rotation_3_1_3 + oMi_rotation_2_3_2 * limi_rotation_3_2_3 + oMi_rotation_2_3_3 * limi_rotation_3_3_3

        val oMi_translation_3_1 = oMi_rotation_2_1_1 * limi_translation_3_1 + oMi_rotation_2_1_2 * limi_translation_3_2 + oMi_rotation_2_1_3 * limi_translation_3_3 + oMi_translation_2_1
        val oMi_translation_3_2 = oMi_rotation_2_2_1 * limi_translation_3_1 + oMi_rotation_2_2_2 * limi_translation_3_2 + oMi_rotation_2_2_3 * limi_translation_3_3 + oMi_translation_2_2
        val oMi_translation_3_3 = oMi_rotation_2_3_1 * limi_translation_3_1 + oMi_rotation_2_3_2 * limi_translation_3_2 + oMi_rotation_2_3_3 * limi_translation_3_3 + oMi_translation_2_3
        
        // let's test until now
        oMi_translation_3_1 + oMi_translation_3_2 + oMi_translation_3_3 + oMi_rotation_3_1_1 + oMi_rotation_3_1_2 + oMi_rotation_3_1_3 + oMi_rotation_3_2_1 + oMi_rotation_3_2_2 + oMi_rotation_3_2_3 + oMi_rotation_3_3_1 + oMi_rotation_3_3_2 + oMi_rotation_3_3_3



    } ensuring(res => res +/- 5e-1)



  

}


