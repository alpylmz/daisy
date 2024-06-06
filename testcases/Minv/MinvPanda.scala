import daisy.lang._
import Real._




object Minv {

  /*
  def alphaSkewSquare(m: Real, x: Real, y: Real, z: Real) = {
    require(m > 0 && m < 5.0 && x > -0.12 && y > -0.12 && z > -0.12 && x < 0.18 && y < 0.18 && z < 0.18)
        // alphaskewsquare returns a skew-symmetric matrix
        val m_1_1 = -m * (y * y + z * z)
        val m_1_2 = m * x * y
        val m_1_3 = m * x * z
        val m_2_1 = m_1_2
        val m_2_2 = -m * (x * x + z * z)
        val m_2_3 = m * y * z
        val m_3_1 = m_1_3
        val m_3_2 = m_2_3
        val m_3_3 = -m * (x * x + y * y)
        m_1_1 + m_1_2 + m_1_3 + m_2_1 + m_2_2 + m_2_3 + m_3_1 + m_3_2 + m_3_3
        // res cannot be a vector or a matrix, so I am just putting one value here as a placeholder
  } ensuring(res => res +/- 1e-2)
  */
  
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

}


