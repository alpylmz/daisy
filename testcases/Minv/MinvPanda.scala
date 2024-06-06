import daisy.lang._
import Real._




object Minv {

  def func(x: Real): Real = {
    require(x >= 0.0 && x <= 1.0)
    x * x + x
  } ensuring(res => res +/- 1e-3)

}


