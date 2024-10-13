import daisy.lang._
import Real._
import daisy.lang.Vector._

import scala.language.experimental.macros

object SinTest {
    

    // For example, to convey that the first element in the first and second row of a matrix 
    // m should have the range [−0.5, 0.5], we write m.specM(Set(Set((0,0),(1,0)),(-0.5,0.5)))
	def sinTest(x: Vector): Real = {
    require(x >= -62.54 && x <= 15.02 && x.size(4)
        && x.specV(Set(
            ((0, 0),(-41.52, -33.01)), 
            ((1, 1),(-53.79, -12.55)), 
            ((2, 2),(-16.71, -9.39)),
            ((3, 3),(-33.75, -9.13))
        ))
    )
        val xx = x * 2
        val xx2 = xx.sin()
        xx2.at(2)
    }


}





