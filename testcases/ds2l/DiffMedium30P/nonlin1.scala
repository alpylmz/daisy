import daisy.lang._
import Real._
import daisy.lang.Vector._

object nonlin1 {

	def nonlin1(x: Vector, y: Vector): Vector = {
require(x >= 0.0 && x <= 1.0 && x.size(1000)
	 && x.specV(Set(((0, 9),(0.23, 0.29)), ((10, 10),(0.32, 0.58)), ((11, 14),(0.15, 0.56)),
((15, 15),(0.32, 0.91)), ((19, 28),(0.15, 0.84)), ((29, 29),(0.8, 0.98)),
((30, 30),(0.03, 0.39)), ((31, 31),(0.38, 0.64)), ((32, 32),(0.22, 0.55)),
((33, 36),(0.09, 0.46)), ((37, 37),(0.43, 0.73)), ((38, 43),(0.11, 0.68)),
((44, 44),(0.12, 0.83)), ((45, 53),(0.92, 1.0)), ((54, 55),(0.53, 0.94)),
((56, 62),(0.29, 0.35)), ((63, 66),(0.5, 0.75)), ((67, 67),(0.09, 0.93)),
((68, 71),(0.48, 0.86)), ((72, 72),(0.54, 0.6)), ((73, 74),(0.09, 0.14)),
((75, 75),(0.27, 0.89)), ((76, 76),(0.23, 0.85)), ((77, 77),(0.03, 0.92)),
((78, 84),(0.6, 0.97)), ((85, 86),(0.34, 0.94)), ((87, 96),(0.05, 0.66)),
((97, 98),(0.33, 0.45)), ((99, 99),(0.25, 0.74)), ((100, 109),(0.18, 0.79)),
((110, 111),(0.07, 0.3)), ((112, 114),(0.1, 0.56)), ((115, 115),(0.55, 0.79)),
((116, 116),(0.46, 0.6)), ((117, 117),(0.03, 0.51)), ((118, 118),(0.1, 0.65)),
((119, 123),(0.13, 0.58)), ((124, 126),(0.35, 0.37)), ((127, 127),(0.16, 0.66)),
((128, 137),(0.17, 0.94)), ((138, 142),(0.33, 0.84)), ((143, 143),(0.48, 0.87)),
((144, 144),(0.09, 0.15)), ((145, 148),(0.12, 0.88)), ((149, 149),(0.5, 0.54)),
((150, 151),(0.12, 0.47)), ((152, 158),(0.16, 0.54)), ((159, 168),(0.37, 0.51)),
((169, 178),(0.2, 0.4)), ((181, 185),(0.13, 0.2)), ((186, 186),(0.24, 0.87)),
((187, 196),(0.5, 0.89)), ((197, 197),(0.02, 0.94)), ((198, 207),(0.49, 0.79)),
((208, 208),(0.57, 0.69)), ((209, 209),(0.23, 0.75)), ((210, 210),(0.15, 0.26)),
((211, 213),(0.03, 0.39)), ((214, 214),(0.29, 0.52)), ((215, 223),(0.08, 0.89)),
((224, 232),(0.23, 0.58)), ((240, 249),(0.6, 0.78)), ((253, 262),(0.06, 0.71)),
((295, 298),(0.25, 0.6)), ((299, 300),(0.04, 0.98)), ((301, 308),(0.62, 0.69)),
((309, 309),(0.34, 0.81)), ((310, 310),(0.46, 0.81)), ((311, 320),(0.01, 0.92)),
((346, 355),(0.41, 0.94)), ((362, 368),(0.09, 0.41)), ((378, 387),(0.24, 0.73)),
((408, 417),(0.15, 0.68)), ((419, 428),(0.14, 0.34)), ((436, 445),(0.2, 0.22)),
((447, 448),(0.33, 0.98)), ((449, 449),(0.79, 1.0)), ((450, 450),(0.48, 0.5)),
((451, 460),(0.78, 0.91)), ((461, 461),(0.64, 0.75)), ((464, 470),(0.2, 0.63)),
((471, 478),(0.79, 0.85)), ((479, 481),(0.09, 0.72)), ((491, 500),(0.16, 0.35)),
((507, 513),(0.67, 0.97)), ((514, 514),(0.13, 0.75)), ((515, 524),(0.83, 0.94)),
((525, 525),(0.74, 0.9)), ((526, 528),(0.8, 0.83)), ((529, 530),(0.44, 0.59)),
((531, 540),(0.78, 0.83)), ((576, 585),(0.3, 0.87)), ((600, 609),(0.53, 0.96)),
((613, 622),(0.12, 0.95)), ((631, 640),(0.79, 0.97)), ((644, 649),(0.25, 0.84)),
((675, 679),(0.2, 0.23)), ((689, 698),(0.24, 0.46)), ((912, 921),(0.49, 0.91)),
((934, 943),(0.69, 0.71)), ((16, 16),(0.31, 0.92)), ((17, 18),(0.41, 0.71)),
((179, 180),(0.04, 0.09)), ((233, 235),(0.23, 0.64)), ((236, 237),(0.45, 0.87)),
((238, 238),(0.66, 0.82)), ((239, 239),(0.96, 1.0)), ((250, 252),(0.09, 0.88)),
((263, 265),(0.15, 0.4)), ((266, 268),(0.51, 0.61)), ((269, 271),(0.08, 0.09)),
((272, 273),(0.46, 0.62)), ((274, 274),(0.41, 0.47)), ((275, 275),(0.63, 0.7)),
((276, 278),(0.42, 0.71)), ((279, 281),(0.17, 0.87)), ((282, 282),(0.04, 0.21)),
((283, 283),(0.19, 0.59)), ((284, 286),(0.1, 0.62)), ((287, 287),(0.51, 0.57)),
((288, 288),(0.07, 0.69)), ((289, 291),(0.27, 0.73)), ((292, 294),(0.12, 0.38)),
((321, 322),(0.62, 0.84)), ((323, 323),(0.51, 0.81)), ((324, 324),(0.11, 0.4)),
((325, 327),(0.08, 0.1)), ((328, 329),(0.0, 0.81)), ((330, 330),(0.04, 0.39)),
((331, 331),(0.76, 0.99)), ((332, 332),(0.03, 0.36)), ((333, 333),(0.25, 0.98)),
((334, 334),(0.31, 0.99)), ((335, 335),(0.39, 0.92)), ((336, 338),(0.67, 0.79)),
((339, 339),(0.0, 0.35)), ((340, 342),(0.37, 0.69)), ((343, 344),(0.44, 0.96)),
((345, 345),(0.38, 0.41)), ((356, 356),(0.02, 0.61)), ((357, 359),(0.91, 0.95)),
((360, 361),(0.01, 0.03)), ((369, 369),(0.15, 0.31)), ((370, 371),(0.77, 0.87)),
((372, 372),(0.55, 0.94)), ((373, 374),(0.2, 0.23)), ((375, 376),(0.38, 0.45)),
((377, 377),(0.2, 0.92)), ((388, 389),(0.29, 0.75)), ((390, 392),(0.23, 0.28)),
((393, 393),(0.4, 0.65)), ((397, 399),(0.78, 0.95)), ((400, 402),(0.08, 0.2)),
((403, 404),(0.38, 0.95)), ((405, 406),(0.46, 0.72)), ((407, 407),(0.46, 0.83)),
((418, 418),(0.38, 0.78)), ((429, 429),(0.64, 0.99)), ((430, 430),(0.27, 0.95)),
((431, 433),(0.49, 0.87)), ((434, 435),(0.56, 0.64)), ((446, 446),(0.08, 0.86)),
((462, 462),(0.28, 0.49)), ((463, 463),(0.19, 0.82)), ((482, 482),(0.09, 0.78)),
((483, 484),(0.35, 0.46)), ((485, 487),(0.52, 0.72)), ((488, 488),(0.69, 0.85)),
((489, 490),(0.38, 0.52)), ((501, 502),(0.39, 0.87)), ((503, 504),(0.18, 0.3)),
((505, 505),(0.24, 0.25)), ((506, 506),(0.69, 0.81)), ((541, 542),(0.25, 0.42)),
((543, 543),(0.89, 0.96)), ((544, 544),(0.22, 0.94)), ((545, 547),(0.06, 1.0)),
((548, 548),(0.32, 0.95)), ((549, 549),(0.03, 0.69)), ((550, 550),(0.82, 0.92)),
((551, 551),(0.16, 0.57)), ((552, 554),(0.24, 0.94)), ((555, 555),(0.22, 0.87)),
((556, 558),(0.34, 0.61)), ((559, 559),(0.21, 0.3)), ((560, 560),(0.11, 0.87)),
((561, 563),(0.41, 0.63)), ((564, 565),(0.47, 0.87)), ((566, 568),(0.75, 0.77)),
((569, 569),(0.02, 0.81)), ((570, 572),(0.0, 0.79)), ((573, 573),(0.23, 0.73)),
((574, 574),(0.54, 0.87)), ((575, 575),(0.09, 0.89)), ((586, 586),(0.26, 0.82)),
((587, 589),(0.22, 0.79)), ((590, 592),(0.44, 0.57)), ((593, 593),(0.4, 0.74)),
((594, 596),(0.32, 0.76)), ((597, 597),(0.36, 0.7)), ((598, 599),(0.26, 0.71)),
((610, 611),(0.19, 0.5)), ((612, 612),(0.34, 0.42)), ((623, 625),(0.09, 0.31)),
((626, 626),(0.33, 0.35)), ((627, 627),(0.51, 0.9)), ((628, 628),(0.3, 0.85)),
((629, 630),(0.09, 0.13)), ((641, 641),(0.76, 0.89)), ((642, 643),(0.74, 0.91)),
((650, 650),(0.17, 0.4)), ((651, 652),(0.09, 0.4)), ((653, 654),(0.09, 0.77)),
((655, 656),(0.08, 0.44)), ((657, 658),(0.21, 0.55)), ((659, 659),(0.65, 0.94)),
((660, 660),(0.23, 0.32)), ((661, 661),(0.06, 0.19)), ((662, 662),(0.0, 0.03)),
((663, 664),(0.12, 0.54)), ((665, 665),(0.46, 0.91)), ((666, 666),(0.09, 0.49)),
((667, 669),(0.15, 0.7)), ((671, 673),(0.75, 0.98)), ((674, 674),(0.23, 0.35)),
((680, 680),(0.43, 0.44)), ((681, 683),(0.15, 0.28)), ((684, 684),(0.03, 0.88)),
((685, 685),(0.98, 0.98)), ((686, 686),(0.38, 0.69)), ((687, 688),(0.11, 0.21)),
((699, 699),(0.0, 0.33)), ((700, 702),(0.53, 0.8)), ((703, 704),(0.23, 0.48)),
((705, 707),(0.2, 0.54)), ((708, 709),(0.45, 0.93)), ((711, 713),(0.45, 0.99)),
((714, 715),(0.13, 0.72)), ((716, 718),(0.69, 0.92)), ((719, 719),(0.19, 0.44)),
((720, 721),(0.19, 0.66)), ((722, 724),(0.15, 0.46)), ((725, 727),(0.41, 0.97)),
((730, 732),(0.51, 0.6)), ((733, 734),(0.62, 0.65)), ((735, 735),(0.29, 0.6)),
((736, 738),(0.21, 0.61)), ((739, 740),(0.87, 0.93)), ((741, 743),(0.3, 0.49)),
((744, 744),(0.05, 0.59)), ((745, 745),(0.5, 0.78)), ((746, 747),(0.09, 0.97)),
((748, 748),(0.2, 0.48)), ((749, 751),(0.61, 0.96)), ((752, 752),(0.11, 0.13)),
((753, 753),(0.48, 0.79)), ((754, 756),(0.03, 0.38)), ((757, 759),(0.42, 0.71)),
((760, 762),(0.07, 0.18)), ((764, 766),(0.51, 0.76)), ((772, 774),(0.54, 0.68)),
((775, 775),(0.4, 0.76)), ((776, 777),(0.37, 0.92)), ((790, 792),(0.08, 0.78)),
((820, 822),(0.46, 0.53)), ((847, 849),(0.25, 0.94)), ((850, 852),(0.3, 0.32)),
((859, 861),(0.06, 0.89)), ((862, 863),(0.05, 0.83)), ((864, 865),(0.15, 0.29)),
((866, 868),(0.03, 0.5)), ((888, 890),(0.72, 0.85)), ((922, 923),(0.5, 0.98)),
((924, 924),(0.3, 0.32)), ((925, 927),(0.27, 0.6)), ((928, 928),(0.12, 0.47)),
((930, 932),(0.49, 0.97)), ((944, 945),(0.62, 0.88)), ((946, 948),(0.49, 0.86)),
((950, 950),(0.5, 0.7)), ((951, 951),(0.2, 0.59)), ((952, 952),(0.3, 0.5)),
((953, 955),(0.18, 0.5)), ((957, 959),(0.11, 0.64)), ((967, 968),(0.06, 0.45)),
((969, 971),(0.1, 0.54)), ((972, 973),(0.02, 0.82)), ((975, 977),(0.3, 0.81)),
((978, 978),(0.33, 0.95)), ((979, 980),(0.14, 0.67)), ((982, 983),(0.24, 0.81)),
((986, 987),(0.27, 0.48)), ((988, 989),(0.22, 0.59)), ((990, 990),(0.08, 0.26)),
((991, 993),(0.22, 0.24)), ((994, 994),(0.16, 0.42)), ((995, 995),(0.45, 0.59)),
((996, 997),(0.55, 0.68)), ((998, 998),(0.47, 0.61)), ((999, 999),(0.14, 0.85))))
	 && y >= 0.0 && y <= 1.0 && y.size(1000)
	 && y.specV(Set(((60, 69),(0.68, 0.97)), ((97, 98),(0.36, 0.8)), ((185, 194),(0.07, 0.7)),
((213, 222),(0.33, 0.84)), ((253, 262),(0.0, 0.63)), ((263, 266),(0.14, 0.28)),
((269, 269),(0.06, 0.45)), ((270, 279),(0.55, 0.81)), ((284, 293),(0.04, 0.83)),
((318, 327),(0.11, 0.22)), ((331, 331),(0.34, 0.63)), ((333, 334),(0.33, 0.57)),
((335, 340),(0.22, 0.79)), ((341, 348),(0.21, 0.84)), ((349, 358),(0.54, 0.81)),
((359, 364),(0.03, 0.78)), ((365, 368),(0.03, 0.5)), ((369, 378),(0.06, 0.42)),
((384, 385),(0.63, 0.72)), ((386, 386),(0.4, 0.58)), ((387, 391),(0.51, 0.59)),
((396, 405),(0.23, 0.87)), ((428, 437),(0.8, 1.0)), ((438, 443),(0.33, 0.96)),
((449, 458),(0.07, 0.99)), ((461, 463),(0.13, 0.48)), ((464, 466),(0.45, 0.68)),
((467, 469),(0.45, 0.71)), ((473, 475),(0.19, 0.34)), ((480, 489),(0.28, 0.4)),
((490, 491),(0.19, 0.74)), ((492, 492),(0.44, 0.52)), ((493, 493),(0.2, 0.7)),
((494, 495),(0.2, 0.27)), ((496, 496),(0.56, 0.7)), ((497, 504),(0.75, 0.97)),
((505, 512),(0.92, 0.95)), ((520, 525),(0.72, 0.72)), ((542, 549),(0.06, 0.21)),
((551, 551),(0.01, 0.86)), ((555, 563),(0.63, 0.72)), ((564, 564),(0.26, 0.36)),
((565, 566),(0.16, 0.63)), ((567, 576),(0.31, 0.99)), ((577, 581),(0.04, 0.18)),
((582, 583),(0.36, 0.9)), ((584, 585),(0.46, 0.8)), ((586, 595),(0.1, 0.11)),
((596, 605),(0.15, 0.78)), ((679, 688),(0.47, 0.99)), ((689, 698),(0.79, 0.91)),
((699, 701),(0.48, 0.82)), ((702, 703),(0.11, 0.13)), ((704, 707),(0.51, 0.56)),
((708, 710),(0.21, 0.25)), ((711, 720),(0.02, 0.91)), ((721, 723),(0.71, 0.95)),
((724, 724),(0.44, 0.98)), ((725, 734),(0.38, 0.73)), ((735, 738),(0.81, 0.84)),
((739, 748),(0.01, 0.13)), ((749, 751),(0.21, 0.59)), ((752, 752),(0.01, 0.38)),
((753, 756),(0.79, 0.83)), ((757, 757),(0.04, 0.46)), ((758, 761),(0.53, 0.89)),
((762, 763),(0.07, 0.32)), ((764, 773),(0.62, 0.89)), ((774, 775),(0.48, 0.64)),
((776, 776),(0.21, 0.24)), ((778, 779),(0.38, 0.91)), ((780, 788),(0.1, 0.59)),
((789, 790),(0.13, 0.2)), ((791, 792),(0.46, 0.59)), ((793, 797),(0.64, 0.71)),
((798, 800),(0.07, 0.44)), ((801, 803),(0.05, 0.72)), ((804, 806),(0.4, 0.47)),
((807, 807),(0.67, 0.71)), ((808, 809),(0.24, 0.65)), ((810, 810),(0.47, 0.69)),
((811, 820),(0.14, 0.91)), ((823, 823),(0.06, 0.6)), ((824, 824),(0.24, 0.99)),
((825, 825),(0.23, 0.39)), ((826, 826),(0.79, 0.82)), ((827, 827),(0.63, 0.74)),
((828, 836),(0.04, 0.09)), ((837, 838),(0.8, 0.98)), ((839, 840),(0.04, 0.05)),
((841, 841),(0.24, 0.93)), ((842, 849),(0.22, 0.67)), ((850, 852),(0.64, 0.93)),
((853, 854),(0.26, 0.92)), ((855, 863),(0.02, 0.15)), ((864, 873),(0.1, 0.76)),
((874, 881),(0.71, 0.97)), ((882, 886),(0.19, 0.81)), ((887, 895),(0.16, 0.51)),
((896, 905),(0.11, 0.79)), ((0, 2),(0.69, 0.7)), ((3, 5),(0.18, 0.64)),
((7, 8),(0.05, 0.71)), ((9, 11),(0.43, 0.8)), ((12, 14),(0.34, 0.63)),
((27, 29),(0.13, 0.43)), ((33, 33),(0.32, 0.37)), ((41, 43),(0.07, 0.99)),
((46, 46),(0.11, 0.77)), ((47, 48),(0.02, 0.22)), ((49, 49),(0.68, 0.72)),
((50, 50),(0.02, 0.07)), ((51, 51),(0.54, 0.98)), ((52, 52),(0.51, 0.67)),
((53, 53),(0.48, 0.79)), ((54, 54),(0.15, 0.84)), ((55, 57),(0.42, 0.72)),
((58, 59),(0.06, 0.23)), ((70, 70),(0.15, 0.66)), ((71, 73),(0.19, 0.73)),
((77, 79),(0.22, 0.53)), ((80, 80),(0.35, 0.5)), ((81, 82),(0.0, 0.66)),
((83, 83),(0.33, 0.49)), ((84, 84),(0.44, 0.98)), ((85, 87),(0.23, 0.49)),
((88, 90),(0.4, 0.76)), ((94, 94),(0.75, 0.88)), ((95, 96),(0.25, 0.99)),
((99, 99),(0.83, 0.92)), ((100, 100),(0.42, 0.51)), ((101, 103),(0.21, 0.56)),
((107, 108),(0.08, 0.44)), ((109, 109),(0.59, 0.64)), ((110, 110),(0.28, 0.59)),
((111, 111),(0.05, 0.81)), ((112, 112),(0.02, 0.9)), ((113, 115),(0.04, 0.42)),
((116, 117),(0.23, 0.71)), ((118, 120),(0.35, 0.96)), ((121, 122),(0.09, 0.88)),
((123, 123),(0.64, 0.66)), ((125, 127),(0.26, 0.55)), ((128, 130),(0.65, 0.73)),
((131, 131),(0.51, 0.74)), ((132, 132),(0.11, 0.13)), ((133, 135),(0.22, 0.39)),
((136, 138),(0.32, 0.99)), ((140, 141),(0.61, 0.93)), ((142, 142),(0.18, 0.55)),
((143, 143),(0.39, 0.5)), ((144, 145),(0.22, 0.75)), ((146, 148),(0.04, 0.96)),
((152, 154),(0.89, 1.0)), ((164, 166),(0.13, 0.9)), ((167, 169),(0.15, 0.93)),
((171, 172),(0.51, 0.91)), ((175, 177),(0.06, 0.36)), ((178, 180),(0.65, 0.85)),
((195, 195),(0.38, 0.71)), ((196, 198),(0.35, 0.45)), ((199, 201),(0.65, 0.95)),
((202, 204),(0.57, 0.63)), ((205, 205),(0.08, 0.27)), ((206, 206),(0.26, 0.96)),
((207, 207),(0.09, 0.3)), ((208, 208),(0.26, 0.85)), ((209, 209),(0.25, 0.41)),
((210, 210),(0.18, 0.93)), ((211, 211),(0.5, 0.7)), ((212, 212),(0.41, 0.5)),
((223, 223),(0.34, 0.47)), ((224, 224),(0.12, 0.13)), ((225, 225),(0.16, 0.84)),
((226, 228),(0.25, 0.82)), ((229, 229),(0.38, 0.5)), ((230, 231),(0.26, 0.36)),
((232, 232),(0.63, 0.78)), ((233, 234),(0.26, 0.85)), ((235, 237),(0.31, 0.36)),
((238, 240),(0.29, 0.31)), ((241, 242),(0.02, 0.6)), ((243, 245),(0.04, 0.64)),
((246, 246),(0.09, 0.95)), ((247, 247),(0.69, 0.72)), ((248, 248),(0.22, 0.96)),
((249, 251),(0.14, 0.55)), ((267, 268),(0.44, 0.45)), ((280, 281),(0.02, 0.03)),
((282, 283),(0.59, 0.72)), ((294, 296),(0.53, 0.6)), ((297, 299),(0.38, 0.66)),
((300, 300),(0.02, 0.82)), ((301, 302),(0.72, 0.83)), ((303, 305),(0.01, 0.2)),
((306, 308),(0.03, 0.46)), ((309, 309),(0.62, 0.91)), ((310, 310),(0.26, 0.94)),
((311, 311),(0.07, 0.26)), ((312, 312),(0.67, 0.91)), ((313, 315),(0.09, 0.15)),
((316, 316),(0.91, 1.0)), ((317, 317),(0.44, 0.95)), ((328, 329),(0.29, 0.93)),
((330, 330),(0.1, 0.73)), ((332, 332),(0.07, 0.26)), ((379, 379),(0.16, 0.86)),
((380, 380),(0.38, 0.89)), ((381, 383),(0.5, 0.87)), ((392, 394),(0.72, 0.93)),
((395, 395),(0.19, 0.63)), ((406, 408),(0.16, 0.86)), ((409, 409),(0.84, 0.95)),
((410, 410),(0.67, 0.77)), ((411, 411),(0.04, 0.15)), ((412, 413),(0.23, 0.89)),
((415, 417),(0.92, 0.99)), ((418, 419),(0.52, 0.95)), ((420, 420),(0.86, 0.92)),
((421, 421),(0.42, 0.44)), ((422, 424),(0.18, 0.36)), ((425, 425),(0.31, 0.34)),
((426, 427),(0.61, 0.9)), ((444, 445),(0.73, 0.82)), ((446, 447),(0.59, 0.76)),
((448, 448),(0.74, 0.93)), ((459, 459),(0.36, 0.74)), ((460, 460),(0.51, 0.88)),
((470, 470),(0.25, 0.8)), ((471, 471),(0.75, 0.86)), ((472, 472),(0.18, 0.68)),
((476, 478),(0.57, 0.63)), ((479, 479),(0.48, 0.59)), ((513, 514),(0.47, 0.88)),
((526, 526),(0.69, 0.99)), ((527, 529),(0.3, 0.96)), ((530, 530),(0.22, 0.31)),
((531, 531),(0.31, 0.4)), ((532, 533),(0.57, 0.69)), ((534, 536),(0.34, 0.93)),
((539, 541),(0.14, 0.14)), ((550, 550),(0.05, 0.88)), ((552, 552),(0.37, 0.39)),
((553, 554),(0.01, 0.27)), ((628, 630),(0.33, 0.39)), ((631, 633),(0.64, 0.74)),
((634, 634),(0.22, 0.66)), ((635, 635),(0.55, 0.76)), ((637, 638),(0.56, 1.0)),
((639, 641),(0.53, 0.72)), ((642, 642),(0.37, 0.89)), ((643, 645),(0.51, 0.7)),
((648, 650),(0.23, 0.85)), ((651, 653),(0.08, 0.5)), ((654, 656),(0.77, 0.99)),
((657, 659),(0.54, 0.69)), ((660, 662),(0.56, 0.81)), ((664, 664),(0.66, 0.97)),
((665, 666),(0.73, 0.83)), ((667, 669),(0.51, 0.65)), ((670, 670),(0.55, 0.81)),
((671, 673),(0.51, 0.81)), ((674, 674),(0.07, 0.97)), ((675, 675),(0.82, 0.85)),
((676, 678),(0.5, 0.57)), ((777, 777),(0.84, 0.87)), ((821, 822),(0.38, 0.48)),
((906, 906),(0.05, 0.54)), ((907, 909),(0.48, 0.52)), ((910, 910),(0.32, 0.65)),
((911, 913),(0.25, 0.58)), ((914, 914),(0.78, 0.99)), ((915, 917),(0.11, 0.81)),
((918, 920),(0.19, 0.61)), ((924, 926),(0.1, 0.52)), ((927, 927),(0.26, 0.67)),
((929, 930),(0.35, 0.36)), ((931, 933),(0.46, 0.95)), ((940, 942),(0.09, 0.26)),
((948, 949),(0.61, 0.7)), ((959, 961),(0.15, 0.89)), ((962, 963),(0.27, 0.56)),
((964, 966),(0.12, 0.43)), ((967, 967),(0.15, 0.94)), ((968, 968),(0.31, 0.64)),
((969, 971),(0.3, 0.36)), ((972, 974),(0.65, 0.78)), ((975, 975),(0.5, 0.83)),
((976, 978),(0.53, 1.0)), ((979, 979),(0.32, 0.82)), ((980, 981),(0.27, 0.64)),
((982, 983),(0.72, 0.9)), ((984, 985),(0.18, 0.97)), ((986, 987),(0.37, 0.67)),
((988, 988),(0.13, 0.16)), ((989, 990),(0.17, 0.81)), ((991, 991),(0.55, 0.7)),
((992, 994),(0.58, 0.84)), ((995, 997),(0.5, 0.87)), ((998, 999),(0.45, 0.7))))
	)

        //x := x + 0.01 * (-2*x - 3*y + x*x)
        val x1: Real = y.fold(x.head)((acc: Real, yi: Real) => {acc + 0.01 * (-2*acc - 3*yi + acc*acc)})
        //y := y + 0.01 * (x + y)
        val y1: Real = x.fold(y.head)((acc: Real, xi: Real) => {acc + 0.01 * (xi + acc)})
        Vector(List(x1, y1))
    }


}