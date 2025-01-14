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
((983, 991),(0.07, 0.54)), ((15, 15),(0.36, 0.7)), ((16, 16),(0.2, 0.3)),
((33, 33),(0.23, 0.95)), ((34, 35),(0.33, 0.52)), ((68, 68),(0.74, 0.82)),
((75, 77),(0.55, 0.9)), ((78, 80),(0.35, 0.49)), ((81, 83),(0.37, 0.54)),
((84, 85),(0.43, 0.5)), ((96, 96),(0.45, 0.64)), ((97, 97),(0.39, 0.89)),
((98, 98),(0.33, 0.59)), ((114, 114),(0.18, 0.52)), ((174, 174),(0.35, 0.45)),
((175, 175),(0.66, 0.84)), ((176, 178),(0.61, 0.94)), ((195, 195),(0.52, 0.54)),
((218, 219),(0.24, 0.34)), ((229, 230),(0.26, 0.66)), ((241, 242),(0.45, 0.92)),
((243, 243),(0.12, 0.23)), ((244, 245),(0.63, 0.73)), ((246, 248),(0.67, 0.84)),
((276, 276),(0.38, 0.97)), ((277, 277),(0.03, 0.43)), ((278, 279),(0.01, 0.69)),
((315, 315),(0.18, 0.58)), ((317, 317),(0.46, 0.8)), ((318, 319),(0.47, 0.88)),
((320, 320),(0.18, 0.41)), ((331, 332),(0.31, 0.47)), ((333, 334),(0.24, 0.62)),
((335, 337),(0.41, 0.66)), ((338, 339),(0.49, 0.88)), ((340, 342),(0.72, 0.88)),
((343, 345),(0.01, 0.62)), ((346, 346),(0.04, 0.51)), ((349, 351),(0.16, 0.43)),
((352, 354),(0.4, 0.85)), ((355, 356),(0.17, 0.82)), ((358, 358),(0.53, 0.96)),
((359, 359),(0.05, 0.19)), ((360, 362),(0.0, 0.86)), ((363, 365),(0.58, 0.93)),
((366, 368),(0.28, 0.52)), ((369, 370),(0.34, 0.54)), ((371, 371),(0.06, 0.39)),
((372, 372),(0.88, 0.95)), ((373, 373),(0.65, 0.89)), ((374, 374),(0.48, 0.92)),
((375, 377),(0.44, 0.65)), ((378, 378),(0.84, 0.92)), ((380, 382),(0.2, 0.87)),
((384, 385),(0.58, 0.64)), ((386, 387),(0.2, 0.6)), ((388, 389),(0.05, 0.78)),
((400, 402),(0.65, 0.97)), ((404, 404),(0.12, 0.72)), ((406, 408),(0.28, 0.65)),
((409, 409),(0.71, 0.94)), ((410, 412),(0.16, 0.53)), ((420, 422),(0.0, 0.33)),
((424, 426),(0.73, 0.88)), ((429, 431),(0.48, 0.58)), ((432, 434),(0.08, 0.27)),
((458, 460),(0.7, 0.86)), ((478, 478),(0.04, 0.44)), ((479, 481),(0.69, 0.79)),
((482, 482),(0.12, 0.88)), ((483, 484),(0.38, 0.76)), ((485, 486),(0.02, 0.29)),
((487, 487),(0.25, 0.63)), ((498, 499),(0.0, 0.54)), ((500, 502),(0.18, 0.83)),
((503, 504),(0.25, 0.64)), ((505, 505),(0.27, 0.4)), ((511, 511),(0.25, 0.72)),
((512, 514),(0.04, 0.98)), ((515, 515),(0.37, 0.52)), ((516, 516),(0.22, 0.79)),
((517, 517),(0.54, 0.97)), ((528, 530),(0.12, 0.23)), ((531, 533),(0.46, 0.47)),
((534, 534),(0.47, 0.73)), ((535, 536),(0.42, 0.51)), ((537, 537),(0.07, 0.87)),
((538, 540),(0.08, 0.67)), ((541, 541),(0.13, 0.84)), ((542, 544),(0.08, 0.51)),
((545, 546),(0.24, 0.54)), ((547, 547),(0.19, 0.38)), ((548, 548),(0.68, 0.92)),
((549, 551),(0.47, 0.88)), ((552, 552),(0.29, 0.86)), ((553, 553),(0.19, 0.96)),
((554, 555),(0.11, 0.81)), ((556, 557),(0.03, 0.37)), ((558, 559),(0.55, 0.91)),
((560, 562),(0.61, 0.9)), ((563, 565),(0.28, 0.93)), ((566, 566),(0.21, 0.33)),
((567, 567),(0.41, 0.6)), ((568, 570),(0.15, 0.84)), ((571, 573),(0.42, 0.89)),
((574, 576),(0.59, 0.83)), ((577, 577),(0.34, 0.94)), ((578, 580),(0.09, 0.38)),
((581, 582),(0.24, 0.55)), ((583, 583),(0.38, 0.46)), ((584, 586),(0.6, 1.0)),
((587, 587),(0.16, 0.34)), ((588, 589),(0.84, 0.86)), ((614, 614),(0.15, 0.5)),
((615, 615),(0.68, 0.78)), ((616, 617),(0.03, 0.67)), ((628, 630),(0.32, 0.55)),
((631, 633),(0.51, 0.83)), ((634, 636),(0.08, 0.36)), ((637, 639),(0.33, 0.99)),
((642, 644),(0.16, 0.79)), ((645, 647),(0.25, 0.63)), ((649, 651),(0.7, 0.7)),
((656, 658),(0.44, 0.62)), ((659, 660),(0.1, 0.89)), ((661, 661),(0.8, 0.85)),
((662, 663),(0.2, 0.82)), ((671, 672),(0.24, 0.31)), ((699, 699),(0.43, 0.76)),
((702, 702),(0.04, 0.15)), ((703, 703),(0.05, 0.41)), ((704, 704),(0.0, 0.63)),
((705, 705),(0.43, 0.83)), ((706, 706),(0.71, 0.8)), ((713, 713),(0.57, 0.93)),
((714, 714),(0.33, 0.7)), ((715, 717),(0.4, 0.8)), ((725, 725),(0.58, 0.63)),
((726, 728),(0.31, 0.7)), ((729, 731),(0.64, 0.66)), ((740, 742),(0.25, 0.7)),
((743, 745),(0.55, 0.64)), ((751, 753),(0.42, 0.92)), ((762, 764),(0.5, 0.93)),
((775, 775),(0.16, 0.94)), ((776, 777),(0.6, 0.7)), ((778, 780),(0.21, 0.41)),
((781, 783),(0.1, 0.21)), ((791, 793),(0.15, 0.75)), ((797, 799),(0.07, 0.43)),
((800, 802),(0.33, 0.44)), ((803, 805),(0.11, 0.93)), ((806, 806),(0.45, 0.8)),
((807, 809),(0.49, 0.89)), ((810, 810),(0.4, 0.92)), ((811, 812),(0.72, 0.98)),
((813, 813),(0.49, 0.88)), ((814, 816),(0.6, 0.99)), ((817, 819),(0.68, 0.93)),
((838, 840),(0.07, 0.37)), ((841, 841),(0.34, 0.49)), ((842, 842),(0.2, 0.84)),
((843, 845),(0.68, 0.76)), ((846, 846),(0.69, 0.71)), ((847, 849),(0.17, 0.62)),
((850, 850),(0.27, 0.98)), ((851, 852),(0.84, 0.97)), ((853, 853),(0.15, 0.64)),
((854, 854),(0.48, 0.6)), ((855, 856),(0.36, 0.92)), ((857, 857),(0.88, 0.95)),
((858, 860),(0.21, 0.7)), ((874, 876),(0.35, 0.86)), ((883, 884),(0.02, 0.85)),
((896, 897),(0.74, 0.77)), ((908, 908),(0.5, 0.69)), ((909, 911),(0.53, 0.73)),
((912, 913),(0.63, 0.64)), ((914, 914),(0.51, 0.55)), ((921, 921),(0.09, 0.26)),
((922, 922),(0.03, 0.87)), ((924, 924),(0.38, 0.73)), ((935, 935),(0.65, 0.81)),
((936, 936),(0.21, 0.87)), ((937, 937),(0.64, 0.65)), ((953, 954),(0.31, 0.63)),
((965, 966),(0.11, 0.64)), ((967, 967),(0.65, 0.87)), ((968, 970),(0.25, 0.39)),
((971, 971),(0.4, 0.46)), ((972, 972),(0.52, 0.74)), ((973, 975),(0.34, 0.64)),
((976, 977),(0.0, 0.7)), ((978, 978),(0.07, 0.86)), ((979, 979),(0.42, 0.82)),
((980, 981),(0.26, 0.59)), ((982, 982),(0.56, 0.83)), ((992, 994),(0.25, 0.37)),
((995, 995),(0.38, 0.55)), ((996, 998),(0.05, 0.06)), ((999, 999),(0.67, 0.89))))
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
((982, 991),(0.85, 0.99)), ((0, 0),(0.34, 0.73)), ((37, 37),(0.51, 0.68)),
((39, 39),(0.02, 0.32)), ((40, 40),(0.31, 0.72)), ((41, 43),(0.19, 0.22)),
((44, 44),(0.06, 0.43)), ((45, 45),(0.15, 0.91)), ((46, 46),(0.55, 0.66)),
((47, 49),(0.0, 0.01)), ((86, 86),(0.68, 0.71)), ((87, 87),(0.63, 0.91)),
((88, 88),(0.6, 0.72)), ((89, 90),(0.11, 0.87)), ((91, 92),(0.85, 0.92)),
((93, 93),(0.36, 0.74)), ((104, 104),(0.61, 0.64)), ((141, 143),(0.17, 0.88)),
((144, 144),(0.08, 0.64)), ((145, 145),(0.06, 0.97)), ((146, 146),(0.66, 0.93)),
((147, 148),(0.53, 0.79)), ((149, 150),(0.03, 0.54)), ((151, 153),(0.02, 0.05)),
((159, 161),(0.16, 0.72)), ((162, 162),(0.62, 0.95)), ((163, 165),(0.04, 0.34)),
((170, 172),(0.48, 0.91)), ((173, 174),(0.09, 0.76)), ((185, 186),(0.13, 0.24)),
((187, 189),(0.03, 0.85)), ((190, 191),(0.55, 0.96)), ((192, 194),(0.43, 0.72)),
((195, 197),(0.12, 0.26)), ((198, 198),(0.17, 0.71)), ((199, 201),(0.12, 0.78)),
((202, 203),(0.03, 0.41)), ((204, 206),(0.9, 0.97)), ((207, 207),(0.63, 0.89)),
((208, 210),(0.61, 0.82)), ((215, 216),(0.2, 0.62)), ((217, 219),(0.55, 0.9)),
((220, 221),(0.23, 0.69)), ((222, 224),(0.28, 0.81)), ((225, 225),(0.19, 0.77)),
((226, 228),(0.55, 0.91)), ((229, 229),(0.52, 0.66)), ((230, 230),(0.57, 0.72)),
((237, 237),(0.43, 0.59)), ((238, 238),(0.4, 0.81)), ((239, 239),(0.34, 0.47)),
((240, 241),(0.37, 0.99)), ((242, 244),(0.89, 0.98)), ((245, 245),(0.27, 0.83)),
((246, 246),(0.11, 0.77)), ((247, 249),(0.52, 0.75)), ((260, 260),(0.75, 0.97)),
((261, 263),(0.04, 0.81)), ((264, 264),(0.7, 0.98)), ((265, 267),(0.53, 0.72)),
((268, 268),(0.68, 0.92)), ((269, 269),(0.45, 0.91)), ((270, 272),(0.55, 0.66)),
((273, 275),(0.33, 0.36)), ((277, 278),(0.68, 0.89)), ((279, 281),(0.33, 0.81)),
((282, 283),(0.15, 0.98)), ((284, 285),(0.0, 0.55)), ((286, 286),(0.05, 0.87)),
((287, 289),(0.07, 0.79)), ((294, 296),(0.01, 0.37)), ((297, 297),(0.76, 0.92)),
((298, 300),(0.04, 0.19)), ((301, 301),(0.34, 0.8)), ((302, 303),(0.74, 0.74)),
((304, 306),(0.59, 0.69)), ((307, 307),(0.05, 0.64)), ((308, 309),(0.64, 0.73)),
((310, 312),(0.11, 0.46)), ((315, 317),(0.13, 0.86)), ((318, 318),(0.47, 0.93)),
((319, 320),(0.14, 0.16)), ((321, 323),(0.3, 0.62)), ((324, 324),(0.01, 0.65)),
((325, 326),(0.52, 0.62)), ((327, 329),(0.12, 0.74)), ((330, 331),(0.7, 0.82)),
((332, 332),(0.24, 0.63)), ((343, 343),(0.15, 0.86)), ((354, 355),(0.56, 0.64)),
((356, 357),(0.28, 0.73)), ((358, 358),(0.04, 0.64)), ((359, 361),(0.29, 0.63)),
((362, 363),(0.21, 0.95)), ((364, 365),(0.95, 0.96)), ((366, 367),(0.73, 0.84)),
((378, 378),(0.46, 0.46)), ((389, 390),(0.48, 0.77)), ((416, 416),(0.65, 0.8)),
((417, 417),(0.0, 0.22)), ((428, 428),(0.22, 0.52)), ((429, 431),(0.19, 0.95)),
((432, 433),(0.4, 0.78)), ((434, 435),(0.58, 0.61)), ((436, 438),(0.51, 0.89)),
((439, 439),(0.06, 0.94)), ((440, 441),(0.21, 0.84)), ((452, 452),(0.3, 0.65)),
((453, 453),(0.37, 0.96)), ((454, 454),(0.19, 0.78)), ((457, 457),(0.03, 0.5)),
((458, 460),(0.46, 0.96)), ((461, 461),(0.44, 0.91)), ((462, 463),(0.69, 0.92)),
((475, 475),(0.29, 0.49)), ((476, 476),(0.9, 0.94)), ((510, 510),(0.4, 0.79)),
((511, 513),(0.26, 0.83)), ((514, 514),(0.63, 0.66)), ((515, 515),(0.02, 0.07)),
((551, 551),(0.59, 0.99)), ((552, 553),(0.07, 0.28)), ((554, 555),(0.42, 0.44)),
((578, 579),(0.52, 0.63)), ((617, 617),(0.06, 0.17)), ((637, 639),(0.87, 0.95)),
((640, 642),(0.87, 0.89)), ((653, 655),(0.03, 0.92)), ((656, 658),(0.2, 0.43)),
((659, 660),(0.63, 1.0)), ((661, 662),(0.54, 0.79)), ((663, 663),(0.06, 0.79)),
((664, 666),(0.03, 0.08)), ((667, 668),(0.15, 0.82)), ((669, 670),(0.61, 0.84)),
((671, 671),(0.5, 0.92)), ((672, 674),(0.04, 0.59)), ((675, 677),(0.57, 0.61)),
((696, 698),(0.47, 0.75)), ((699, 701),(0.5, 0.67)), ((702, 702),(0.6, 0.61)),
((703, 705),(0.57, 0.94)), ((706, 708),(0.31, 0.49)), ((709, 711),(0.32, 0.62)),
((717, 719),(0.45, 0.56)), ((720, 721),(0.23, 0.29)), ((722, 724),(0.42, 0.86)),
((725, 726),(0.02, 0.03)), ((729, 731),(0.04, 0.5)), ((732, 732),(0.41, 0.53)),
((733, 734),(0.31, 0.96)), ((735, 735),(0.1, 0.97)), ((736, 736),(0.02, 0.08)),
((737, 739),(0.19, 0.63)), ((740, 742),(0.85, 0.87)), ((743, 745),(0.33, 0.79)),
((746, 746),(0.12, 0.82)), ((757, 758),(0.31, 0.6)), ((759, 759),(0.54, 0.88)),
((760, 762),(0.73, 0.86)), ((763, 764),(0.23, 0.52)), ((765, 765),(0.66, 0.77)),
((766, 768),(0.63, 0.86)), ((769, 771),(0.13, 0.48)), ((772, 772),(0.29, 0.38)),
((773, 775),(0.56, 0.66)), ((776, 776),(0.32, 1.0)), ((781, 781),(0.08, 0.79)),
((782, 784),(0.12, 0.32)), ((790, 792),(0.04, 0.61)), ((814, 816),(0.61, 0.79)),
((845, 845),(0.76, 0.91)), ((856, 856),(0.18, 0.28)), ((857, 858),(0.88, 0.9)),
((859, 859),(0.32, 0.54)), ((860, 860),(0.29, 0.4)), ((924, 924),(0.33, 0.37)),
((925, 926),(0.12, 0.66)), ((927, 927),(0.18, 0.58)), ((928, 930),(0.44, 0.94)),
((936, 938),(0.34, 0.89)), ((941, 943),(0.14, 0.69)), ((944, 946),(0.6, 1.0)),
((947, 947),(0.14, 0.43)), ((948, 948),(0.02, 0.38)), ((951, 953),(0.14, 0.99)),
((964, 964),(0.61, 0.62)), ((965, 967),(0.0, 0.01)), ((968, 968),(0.59, 0.84)),
((969, 969),(0.89, 0.99)), ((970, 970),(0.34, 0.69)), ((971, 973),(0.14, 0.32)),
((974, 974),(0.92, 1.0)), ((975, 975),(0.07, 0.91)), ((976, 976),(0.75, 0.89)),
((977, 978),(0.45, 0.88)), ((980, 981),(0.6, 0.92)), ((992, 992),(0.15, 0.75)),
((993, 993),(0.5, 0.59)), ((994, 996),(0.36, 0.95)), ((997, 999),(0.74, 0.82))))
	)

        //x := x + 0.01 * (-x + y*y)
        val x1: Real = y.fold(x.head)((acc: Real, yi: Real) => {acc + 0.01 * (-acc + yi*yi)})
        //y := y + 0.01 * (-2.0*y + 3.0*x*x)
        val y1: Real = x.fold(y.head)((acc: Real, xi: Real) => {acc + 0.01 * (-2.0*acc + 3.0*xi*xi)})
        Vector(List(x1, y1))
    }


}