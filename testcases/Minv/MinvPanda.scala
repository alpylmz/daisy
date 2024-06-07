import daisy.lang._
import Real._




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
  

}


