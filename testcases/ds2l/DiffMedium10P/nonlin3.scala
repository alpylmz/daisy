import daisy.lang._
import Real._
import daisy.lang.Vector._

object nonlin3 {

	def nonlin3(x: Vector, y: Vector): Vector = {
require(x >= 0.0 && x <= 1.0 && x.size(1000)
	 && x.specV(Set(((0, 1),(0.53, 0.63)), ((2, 11),(0.44, 0.85)), ((12, 12),(0.1, 0.91)),
((13, 14),(0.68, 0.81)), ((17, 17),(0.23, 0.74)), ((18, 27),(0.46, 0.82)),
((28, 28),(0.92, 0.96)), ((29, 32),(0.16, 0.68)), ((36, 37),(0.41, 0.55)),
((38, 40),(0.14, 0.17)), ((41, 45),(0.47, 0.49)), ((46, 46),(0.27, 0.37)),
((47, 54),(0.55, 0.76)), ((55, 64),(0.16, 0.59)), ((65, 67),(0.37, 0.89)),
((69, 74),(0.96, 0.98)), ((86, 95),(0.14, 0.58)), ((99, 102),(0.39, 0.97)),
((103, 103),(0.31, 0.55)), ((104, 108),(0.03, 0.2)), ((109, 109),(0.12, 0.28)),
((110, 113),(0.56, 0.63)), ((115, 116),(0.69, 0.95)), ((117, 126),(0.36, 0.95)),
((127, 127),(0.54, 0.63)), ((128, 129),(0.67, 0.86)), ((130, 131),(0.36, 0.6)),
((132, 132),(0.3, 0.97)), ((133, 133),(0.24, 0.97)), ((134, 134),(0.01, 0.06)),
((135, 140),(0.21, 0.55)), ((141, 141),(0.12, 0.51)), ((142, 151),(0.67, 0.84)),
((152, 152),(0.68, 0.93)), ((153, 153),(0.68, 0.99)), ((154, 154),(0.0, 0.71)),
((155, 158),(0.05, 0.43)), ((159, 159),(0.31, 0.97)), ((160, 160),(0.01, 0.57)),
((161, 170),(0.42, 0.9)), ((171, 173),(0.59, 0.82)), ((179, 188),(0.01, 0.2)),
((189, 192),(0.18, 0.83)), ((193, 194),(0.1, 0.93)), ((196, 196),(0.27, 0.88)),
((197, 203),(0.32, 0.52)), ((204, 205),(0.41, 0.97)), ((206, 207),(0.07, 0.7)),
((208, 209),(0.83, 0.98)), ((210, 217),(0.11, 0.75)), ((220, 228),(0.15, 0.27)),
((231, 240),(0.36, 0.66)), ((249, 252),(0.95, 0.99)), ((253, 261),(0.62, 0.99)),
((262, 271),(0.16, 0.65)), ((272, 274),(0.55, 0.73)), ((275, 275),(0.36, 0.9)),
((280, 282),(0.14, 0.26)), ((283, 292),(0.15, 0.16)), ((293, 295),(0.59, 0.8)),
((296, 297),(0.16, 0.5)), ((298, 301),(0.56, 0.57)), ((302, 302),(0.41, 0.54)),
((303, 304),(0.02, 0.06)), ((305, 314),(0.24, 0.42)), ((316, 316),(0.24, 0.76)),
((321, 330),(0.24, 0.82)), ((468, 477),(0.09, 0.48)), ((488, 497),(0.34, 0.39)),
((518, 527),(0.22, 0.77)), ((590, 599),(0.67, 0.83)), ((600, 601),(0.45, 0.5)),
((602, 602),(0.6, 0.92)), ((603, 603),(0.07, 0.18)), ((604, 613),(0.08, 0.33)),
((618, 627),(0.09, 0.65)), ((664, 668),(0.31, 0.42)), ((669, 670),(0.51, 0.94)),
((673, 682),(0.15, 0.24)), ((683, 683),(0.31, 0.32)), ((684, 686),(0.71, 0.91)),
((687, 688),(0.17, 0.43)), ((689, 698),(0.05, 0.12)), ((700, 701),(0.65, 0.85)),
((707, 708),(0.16, 0.63)), ((709, 710),(0.06, 0.51)), ((711, 712),(0.1, 0.34)),
((765, 774),(0.72, 0.74)), ((828, 837),(0.17, 0.51)), ((861, 863),(0.43, 0.67)),
((864, 873),(0.09, 0.66)), ((886, 895),(0.0, 0.9)), ((898, 907),(0.22, 0.4)),
((915, 920),(0.09, 0.2)), ((923, 923),(0.69, 0.99)), ((925, 934),(0.27, 0.33)),
((938, 942),(0.22, 0.67)), ((943, 952),(0.4, 0.69)), ((955, 964),(0.16, 0.43)),
((983, 991),(0.07, 0.54))))
	 && y >= 0.0 && y <= 1.0 && y.size(1000)
	 && y.specV(Set(((1, 1),(0.62, 0.72)), ((2, 4),(0.37, 0.84)), ((5, 5),(0.64, 0.98)),
((6, 6),(0.14, 0.67)), ((7, 8),(0.11, 0.16)), ((9, 9),(0.02, 0.9)),
((10, 11),(0.0, 0.22)), ((12, 18),(0.2, 1.0)), ((19, 23),(0.36, 0.53)),
((24, 27),(0.17, 0.26)), ((28, 36),(0.16, 0.7)), ((38, 38),(0.82, 0.92)),
((50, 59),(0.06, 0.38)), ((60, 61),(0.32, 0.89)), ((62, 62),(0.04, 0.4)),
((63, 72),(0.55, 0.8)), ((73, 74),(0.07, 0.54)), ((75, 75),(0.17, 0.19)),
((76, 85),(0.5, 0.81)), ((94, 103),(0.7, 0.77)), ((105, 107),(0.66, 0.87)),
((108, 108),(0.29, 0.53)), ((109, 109),(0.72, 0.73)), ((110, 118),(0.11, 0.27)),
((119, 121),(0.2, 0.3)), ((122, 123),(0.79, 0.97)), ((124, 128),(0.83, 0.94)),
((129, 130),(0.67, 0.74)), ((131, 140),(0.13, 0.32)), ((175, 184),(0.49, 0.85)),
((231, 236),(0.16, 0.55)), ((250, 259),(0.06, 0.94)), ((333, 342),(0.08, 0.62)),
((344, 353),(0.59, 0.69)), ((368, 377),(0.15, 0.51)), ((379, 388),(0.07, 0.81)),
((391, 400),(0.91, 0.95)), ((401, 403),(0.02, 0.62)), ((404, 404),(0.09, 0.23)),
((405, 408),(0.45, 0.83)), ((409, 409),(0.18, 0.24)), ((410, 415),(0.31, 0.63)),
((418, 427),(0.13, 0.6)), ((442, 451),(0.37, 0.91)), ((455, 455),(0.02, 0.72)),
((456, 456),(0.05, 0.7)), ((465, 474),(0.3, 0.67)), ((477, 477),(0.09, 0.73)),
((478, 478),(0.2, 0.96)), ((479, 483),(0.6, 0.97)), ((484, 485),(0.01, 0.55)),
((486, 488),(0.06, 0.29)), ((489, 489),(0.31, 0.95)), ((490, 490),(0.48, 0.66)),
((491, 500),(0.6, 0.88)), ((501, 501),(0.38, 0.54)), ((502, 504),(0.38, 0.81)),
((505, 505),(0.83, 0.94)), ((506, 509),(0.22, 0.27)), ((516, 516),(0.51, 0.95)),
((517, 517),(0.13, 0.31)), ((518, 523),(0.05, 0.09)), ((524, 532),(0.44, 0.79)),
((533, 542),(0.35, 0.46)), ((543, 550),(0.48, 0.62)), ((556, 558),(0.77, 0.8)),
((559, 568),(0.4, 0.71)), ((569, 569),(0.16, 0.5)), ((570, 570),(0.91, 0.93)),
((571, 571),(0.16, 0.76)), ((572, 572),(0.18, 0.43)), ((573, 573),(0.21, 0.97)),
((574, 575),(0.65, 0.95)), ((576, 577),(0.25, 0.57)), ((580, 581),(0.26, 0.9)),
((582, 582),(0.12, 0.35)), ((583, 592),(0.28, 0.59)), ((593, 598),(0.32, 0.84)),
((599, 600),(0.81, 0.87)), ((601, 602),(0.3, 0.79)), ((603, 605),(0.14, 0.55)),
((606, 606),(0.56, 0.9)), ((607, 616),(0.17, 0.7)), ((618, 619),(0.1, 0.67)),
((620, 629),(0.44, 0.97)), ((630, 636),(0.47, 0.83)), ((643, 652),(0.44, 0.71)),
((686, 695),(0.16, 0.85)), ((747, 756),(0.17, 0.9)), ((777, 780),(0.32, 0.64)),
((825, 829),(0.28, 0.43)), ((830, 830),(0.1, 0.72)), ((831, 831),(0.16, 0.84)),
((832, 834),(0.19, 0.59)), ((835, 844),(0.49, 0.65)), ((846, 855),(0.1, 0.98)),
((861, 870),(0.44, 0.87)), ((914, 923),(0.65, 0.89)), ((954, 963),(0.1, 0.44)),
((982, 991),(0.85, 0.99))))
	)

        //x := x + 0.01 * (-x + y*y)
        val x1: Real = y.fold(x.head)((acc: Real, yi: Real) => {acc + 0.01 * (-acc + yi*yi)})
        //y := y + 0.01 * (-2.0*y + 3.0*x*x)
        val y1: Real = x.fold(y.head)((acc: Real, xi: Real) => {acc + 0.01 * (-2.0*acc + 3.0*xi*xi)})
        Vector(List(x1, y1))
    }


}