import daisy.lang._
import Real._
import daisy.lang.Vector._

object goubault {

	def goubault(x:Vector, y: Real): Real = {
require(54.86 <= y && y <= 359.03
	 && x >= -270.01 && x <= 385.38 && x.size(10000)
	 && x.specV(Set(((0, 5),(57.17, 337.17)), ((6, 7),(14.39, 217.97)), ((8, 9),(-15.67, 325.73)),
((10, 11),(-146.62, -83.63)), ((12, 13),(-49.65, 261.19)), ((14, 23),(35.94, 259.71)),
((24, 33),(-209.19, 280.59)), ((34, 35),(20.32, 347.89)), ((36, 38),(-178.33, 38.5)),
((39, 40),(113.99, 375.79)), ((41, 42),(-94.83, 261.58)), ((44, 53),(-194.01, -138.89)),
((54, 55),(241.48, 323.17)), ((56, 63),(-123.26, 298.47)), ((64, 73),(-60.02, -36.16)),
((76, 78),(107.69, 259.39)), ((81, 89),(-215.08, 110.35)), ((91, 92),(-235.89, -75.05)),
((93, 102),(281.48, 297.73)), ((103, 104),(-246.66, -214.18)), ((105, 107),(-10.24, 261.67)),
((108, 108),(-94.11, 376.43)), ((109, 118),(-26.24, 266.37)), ((119, 120),(-129.85, 92.84)),
((122, 122),(-228.33, -146.94)), ((123, 123),(21.68, 284.2)), ((124, 132),(97.19, 116.13)),
((133, 134),(-64.5, 331.37)), ((135, 136),(-168.7, 194.18)), ((137, 137),(44.66, 47.95)),
((138, 139),(-224.2, 30.04)), ((140, 149),(-12.34, 364.86)), ((150, 150),(75.52, 284.92)),
((151, 152),(-90.41, 199.66)), ((153, 156),(-172.5, 165.5)), ((157, 163),(-248.41, -84.05)),
((165, 166),(-219.59, -121.93)), ((167, 167),(14.5, 51.42)), ((169, 178),(81.84, 166.11)),
((179, 180),(-25.36, 280.99)), ((181, 182),(-112.69, 200.7)), ((183, 183),(-208.59, 17.55)),
((186, 189),(-35.81, 142.93)), ((190, 199),(131.34, 321.85)), ((200, 201),(-103.88, 319.63)),
((202, 207),(-178.49, 202.93)), ((208, 212),(-134.26, -31.82)), ((213, 213),(-32.62, 146.1)),
((214, 223),(-175.39, 248.89)), ((225, 225),(-254.92, 298.51)), ((226, 226),(-160.55, -15.86)),
((230, 236),(29.44, 211.45)), ((239, 241),(-235.76, 28.42)), ((242, 251),(-209.43, 365.06)),
((252, 256),(121.44, 150.65)), ((257, 262),(-66.37, 327.41)), ((263, 264),(170.86, 249.0)),
((265, 265),(-222.63, 243.75)), ((266, 275),(112.59, 383.72)), ((276, 283),(-57.06, 32.14)),
((289, 290),(37.83, 130.58)), ((291, 293),(-103.59, -31.44)), ((294, 303),(-21.25, 211.85)),
((304, 304),(-102.62, -76.91)), ((307, 309),(168.17, 239.25)), ((310, 310),(-212.29, 295.05)),
((312, 312),(5.76, 199.56)), ((313, 313),(41.63, 179.75)), ((314, 323),(-137.36, 181.58)),
((324, 324),(112.32, 122.06)), ((325, 325),(63.24, 302.73)), ((326, 327),(-253.72, 221.04)),
((328, 328),(-257.24, 118.0)), ((329, 338),(130.23, 375.96)), ((339, 339),(4.17, 309.82)),
((340, 341),(42.01, 191.55)), ((342, 346),(-147.26, 142.66)), ((347, 356),(54.37, 91.99)),
((357, 358),(-140.86, 43.98)), ((359, 362),(71.06, 208.89)), ((363, 363),(-23.59, 203.5)),
((364, 365),(-57.03, 328.68)), ((366, 367),(-225.99, -68.79)), ((368, 371),(-124.25, 309.18)),
((372, 372),(-241.25, -22.31)), ((373, 382),(-39.98, -12.37)), ((383, 383),(-210.02, -65.0)),
((384, 384),(-28.5, 141.2)), ((385, 385),(-234.64, 230.95)), ((386, 387),(49.41, 71.18)),
((388, 390),(247.2, 299.83)), ((391, 391),(-265.7, -87.14)), ((392, 401),(81.88, 189.77)),
((402, 402),(-260.88, 73.25)), ((403, 405),(-192.59, 14.53)), ((406, 406),(76.43, 199.03)),
((407, 409),(-205.24, 356.16)), ((410, 410),(-196.13, -61.0)), ((411, 412),(-144.59, -5.53)),
((413, 413),(14.16, 303.08)), ((414, 414),(-132.97, -13.09)), ((415, 417),(26.6, 316.63)),
((418, 419),(2.8, 88.52)), ((420, 429),(337.99, 375.57)), ((430, 430),(-129.94, 308.88)),
((431, 433),(-234.81, 233.91)), ((436, 438),(-46.42, 158.12)), ((439, 442),(-187.49, 175.81)),
((443, 443),(-260.53, -124.81)), ((444, 444),(-39.93, 325.73)), ((445, 446),(-197.57, 51.71)),
((447, 451),(218.61, 338.16)), ((452, 452),(83.7, 295.53)), ((453, 453),(287.6, 302.02)),
((454, 461),(-230.66, -108.97)), ((462, 463),(-260.3, -171.99)), ((464, 466),(65.06, 88.04)),
((467, 467),(-211.65, 343.64)), ((468, 477),(-24.59, 10.97)), ((479, 485),(-243.72, 14.46)),
((486, 495),(-47.67, 246.29)), ((507, 508),(-143.75, 242.55)), ((515, 516),(-6.13, 147.54)),
((517, 517),(-126.92, 365.1)), ((518, 527),(-127.34, -7.46)), ((545, 554),(-113.79, 201.67)),
((555, 557),(52.22, 294.53)), ((558, 567),(35.97, 321.58)), ((569, 569),(154.92, 356.6)),
((570, 574),(15.24, 170.02)), ((584, 593),(-209.17, 79.83)), ((598, 605),(-44.65, 47.41)),
((606, 608),(21.49, 77.5)), ((610, 619),(-15.19, 376.82)), ((620, 628),(-145.23, 6.18)),
((631, 632),(-86.89, 81.24)), ((635, 644),(42.25, 251.95)), ((645, 645),(-56.72, 120.98)),
((646, 646),(-75.56, 331.42)), ((652, 661),(-152.2, -65.5)), ((667, 671),(-94.23, 0.72)),
((695, 704),(139.56, 324.36)), ((709, 709),(-225.76, 82.69)), ((718, 727),(-6.26, 151.63)),
((728, 732),(-139.23, -67.88)), ((733, 733),(-6.15, 342.96)), ((734, 743),(-161.1, -13.23)),
((745, 746),(-166.22, 100.01)), ((751, 755),(-127.55, -7.0)), ((759, 768),(-30.95, 41.99)),
((784, 789),(21.07, 249.84)), ((790, 793),(-19.19, 243.9)), ((795, 804),(47.45, 376.85)),
((805, 805),(26.02, 210.57)), ((806, 806),(101.27, 227.52)), ((807, 816),(-89.06, 2.47)),
((910, 919),(-246.0, -140.07)), ((1022, 1031),(-44.57, 267.45)), ((1032, 1033),(122.06, 166.81)),
((1034, 1043),(-145.72, 127.72)), ((1050, 1053),(-174.46, -123.33)), ((1055, 1059),(108.59, 178.13)),
((1077, 1085),(-94.29, 304.77)), ((1092, 1101),(33.39, 364.98)), ((1108, 1109),(-269.94, 4.76)),
((1129, 1138),(-94.99, 50.11)), ((1176, 1185),(-221.75, 129.9)), ((1199, 1208),(32.42, 270.11)),
((1217, 1226),(56.69, 179.1)), ((1291, 1297),(-103.21, 14.89)), ((1408, 1417),(263.14, 267.53)),
((1441, 1444),(31.32, 253.9)), ((1458, 1467),(-79.76, 351.42)), ((1505, 1514),(251.49, 294.72)),
((1961, 1970),(-153.01, 191.72)), ((1971, 1971),(87.58, 309.58)), ((1972, 1978),(-110.99, 367.99)),
((1979, 1979),(-177.61, 276.73)), ((1980, 1986),(-59.22, -42.43)), ((1987, 1987),(-180.68, -49.69)),
((1988, 1991),(-264.02, 363.71)), ((1992, 1992),(98.07, 304.39)), ((1993, 1994),(179.94, 326.97)),
((1995, 1995),(127.09, 203.53)), ((1996, 2005),(-175.51, -17.38)), ((2006, 2006),(40.19, 209.68)),
((2007, 2016),(-147.95, 144.46)), ((2017, 2021),(-119.15, 356.91)), ((2022, 2025),(245.26, 282.86)),
((2026, 2029),(10.57, 230.4)), ((2030, 2033),(-147.08, 44.21)), ((2034, 2034),(-32.56, -18.66)),
((2035, 2035),(114.85, 182.76)), ((2036, 2037),(118.72, 244.05)), ((2038, 2039),(42.18, 287.51)),
((2040, 2040),(-174.12, 5.01)), ((2041, 2041),(-5.99, 2.97)), ((2042, 2042),(130.2, 134.46)),
((2043, 2043),(356.38, 375.0)), ((2044, 2045),(-174.44, 232.5)), ((2046, 2047),(-50.88, 167.79)),
((2048, 2051),(63.89, 299.54)), ((2052, 2053),(-69.72, 232.49)), ((2054, 2056),(158.34, 158.41)),
((2057, 2057),(-156.2, 188.87)), ((2058, 2061),(175.45, 223.82)), ((2062, 2062),(100.38, 277.78)),
((2063, 2064),(191.53, 354.48)), ((2065, 2066),(84.3, 300.76)), ((2067, 2067),(-53.61, -46.86)),
((2068, 2077),(-109.71, -84.2)), ((2078, 2084),(-218.24, -1.09)), ((2085, 2085),(-74.52, 302.42)),
((2086, 2095),(-78.87, 174.39)), ((2096, 2096),(140.65, 363.79)), ((2097, 2097),(239.37, 274.52)),
((2098, 2098),(-263.21, -125.1)), ((2099, 2100),(-146.63, 278.35)), ((2101, 2101),(-111.1, 165.45)),
((2110, 2114),(-267.71, 166.26)), ((2115, 2116),(-145.0, 136.56)), ((2117, 2123),(74.29, 108.25)),
((2124, 2124),(-71.35, 254.04)), ((2125, 2125),(76.06, 154.19)), ((2126, 2127),(150.08, 302.08)),
((2128, 2128),(97.45, 125.92)), ((2129, 2129),(221.64, 378.73)), ((2130, 2130),(-125.88, 290.99)),
((2131, 2136),(-54.7, 70.84)), ((2137, 2137),(-98.01, 331.06)), ((2138, 2141),(-62.63, 358.31)),
((2142, 2142),(85.43, 366.26)), ((2143, 2152),(-44.81, 244.63)), ((2153, 2153),(-177.75, 64.49)),
((2154, 2157),(-61.27, 81.21)), ((2158, 2167),(-46.55, 236.62)), ((2168, 2169),(125.06, 236.7)),
((2170, 2177),(-61.5, 257.13)), ((2178, 2178),(154.49, 168.12)), ((2179, 2181),(-148.15, 161.73)),
((2182, 2183),(-144.72, 82.62)), ((2185, 2189),(-51.2, 168.64)), ((2190, 2190),(-206.8, -117.36)),
((2191, 2191),(51.35, 197.32)), ((2192, 2201),(-9.48, 385.2)), ((2202, 2204),(-142.65, -13.48)),
((2205, 2205),(295.02, 315.92)), ((2206, 2208),(-162.17, 11.66)), ((2209, 2218),(-250.81, 75.54)),
((2219, 2219),(-220.37, 138.58)), ((2220, 2220),(33.36, 169.28)), ((2221, 2221),(-39.82, 194.85)),
((2222, 2222),(259.0, 334.26)), ((2223, 2232),(-187.98, -87.45)), ((2242, 2244),(-251.6, 336.54)),
((2245, 2247),(77.82, 275.0)), ((2248, 2257),(-128.65, 264.06)), ((2259, 2260),(-140.96, 378.02)),
((2261, 2266),(150.18, 231.35)), ((2267, 2269),(98.02, 250.07)), ((2270, 2279),(-239.39, 91.93)),
((2280, 2280),(268.46, 337.08)), ((2283, 2292),(-257.19, 152.23)), ((2293, 2302),(-158.76, 155.94)),
((2303, 2304),(96.05, 245.9)), ((2305, 2307),(-187.12, 54.81)), ((2308, 2308),(-56.83, 76.91)),
((2309, 2318),(-208.91, 322.43)), ((2319, 2328),(-95.05, -18.0)), ((2329, 2330),(-94.07, 124.21)),
((2331, 2333),(-95.31, 243.17)), ((2334, 2334),(37.24, 89.93)), ((2335, 2335),(-156.41, -101.81)),
((2336, 2338),(-140.92, 116.84)), ((2339, 2341),(-238.91, -101.5)), ((2342, 2342),(-254.16, -85.27)),
((2343, 2351),(23.63, 77.96)), ((2352, 2354),(-249.47, 73.1)), ((2355, 2355),(130.77, 183.5)),
((2356, 2365),(-17.41, 144.21)), ((2366, 2366),(-209.62, 366.1)), ((2367, 2376),(76.1, 341.76)),
((2377, 2377),(-18.48, 179.85)), ((2378, 2382),(148.45, 327.41)), ((2383, 2383),(-43.75, 151.36)),
((2384, 2384),(6.03, 259.07)), ((2385, 2394),(-193.32, 328.95)), ((2396, 2397),(-221.29, 324.97)),
((2398, 2399),(-242.47, -179.87)), ((2403, 2412),(-213.7, 256.68)), ((2414, 2414),(129.14, 349.59)),
((2415, 2416),(-112.69, 55.13)), ((2424, 2433),(32.44, 310.86)), ((2435, 2435),(-38.43, 228.11)),
((2437, 2442),(-134.15, 185.23)), ((2443, 2444),(144.42, 201.71)), ((2445, 2445),(-49.76, 142.22)),
((2446, 2455),(-10.96, 246.95)), ((2459, 2468),(-34.92, 227.74)), ((2476, 2485),(-128.79, 2.31)),
((2497, 2506),(-108.06, -38.84)), ((2507, 2516),(-104.28, 144.41)), ((2541, 2550),(-8.33, 164.88)),
((2551, 2555),(-100.44, 185.91)), ((2556, 2556),(149.21, 261.85)), ((2557, 2562),(-94.72, 312.37)),
((2563, 2563),(262.07, 315.25)), ((2564, 2565),(-222.01, 129.4)), ((2566, 2572),(-152.68, -106.62)),
((2573, 2574),(-20.22, 252.8)), ((2575, 2579),(186.94, 211.01)), ((2580, 2580),(73.88, 224.28)),
((2581, 2585),(-197.33, 69.85)), ((2586, 2586),(99.18, 150.18)), ((2587, 2587),(-202.81, -6.57)),
((2588, 2592),(237.02, 260.26)), ((2593, 2593),(-119.03, 224.44)), ((2594, 2603),(-260.48, 98.84)),
((2604, 2605),(-157.83, 112.39)), ((2606, 2606),(-24.38, 156.16)), ((2607, 2611),(-174.05, -128.5)),
((2612, 2615),(-14.18, 87.09)), ((2616, 2617),(-27.29, 278.17)), ((2619, 2625),(268.34, 375.11)),
((2626, 2635),(183.42, 273.91)), ((2636, 2636),(76.05, 276.64)), ((2637, 2638),(211.29, 220.79)),
((2639, 2644),(304.63, 340.72)), ((2645, 2647),(255.74, 369.43)), ((2648, 2648),(44.79, 157.2)),
((2649, 2658),(93.55, 198.22)), ((2659, 2659),(-197.99, -139.46)), ((2660, 2660),(-169.83, 212.03)),
((2661, 2661),(-29.25, 194.25)), ((2662, 2669),(181.82, 183.38)), ((2670, 2675),(-219.93, 140.76)),
((2676, 2685),(-88.4, 335.4)), ((2686, 2688),(-154.0, 307.01)), ((2689, 2689),(-5.17, 62.45)),
((2690, 2692),(249.02, 272.01)), ((2693, 2702),(-158.77, 160.29)), ((2703, 2703),(-91.59, 355.96)),
((2704, 2706),(-249.68, -20.85)), ((2707, 2712),(180.51, 302.79)), ((2713, 2722),(-215.95, 140.71)),
((2723, 2725),(-108.55, -52.83)), ((2726, 2726),(-65.94, -45.21)), ((2727, 2727),(198.33, 354.43)),
((2728, 2728),(-157.92, 324.39)), ((2729, 2738),(-76.62, -22.88)), ((2739, 2739),(55.84, 322.54)),
((2740, 2749),(114.5, 302.8)), ((2750, 2756),(9.47, 148.78)), ((2757, 2760),(-52.21, 81.75)),
((2761, 2761),(112.33, 370.81)), ((2762, 2771),(-93.05, 5.16)), ((2772, 2773),(-83.91, 162.64)),
((2774, 2775),(216.76, 245.08)), ((2776, 2778),(177.99, 331.24)), ((2779, 2788),(98.09, 187.54)),
((2789, 2795),(-232.17, -131.63)), ((2796, 2796),(-121.38, 89.88)), ((2797, 2801),(242.46, 297.08)),
((2802, 2811),(19.33, 299.65)), ((2812, 2813),(-170.99, -40.04)), ((2814, 2819),(213.64, 226.41)),
((2820, 2820),(81.95, 202.34)), ((2821, 2827),(111.63, 139.83)), ((2828, 2831),(272.86, 282.79)),
((2832, 2833),(-53.16, 61.78)), ((2834, 2835),(199.86, 254.44)), ((2836, 2845),(-236.96, 310.55)),
((2846, 2846),(-5.2, 193.95)), ((2847, 2856),(-224.97, 256.14)), ((2857, 2857),(131.46, 380.92)),
((2858, 2858),(-251.87, -250.93)), ((2859, 2860),(3.84, 81.0)), ((2861, 2864),(19.83, 242.62)),
((2865, 2865),(124.21, 370.31)), ((2866, 2874),(-193.07, 177.09)), ((2875, 2875),(172.85, 280.27)),
((2876, 2878),(-214.73, -134.06)), ((2879, 2879),(-208.85, 50.6)), ((2880, 2880),(249.36, 280.18)),
((2881, 2885),(-109.99, -102.33)), ((2886, 2886),(23.62, 110.08)), ((2887, 2887),(73.71, 266.6)),
((2888, 2890),(-254.88, -217.72)), ((2891, 2891),(-88.28, -8.42)), ((2892, 2892),(-25.21, 366.06)),
((2893, 2893),(-111.59, -11.51)), ((2894, 2903),(13.55, 83.74)), ((2904, 2904),(-19.82, -14.17)),
((2905, 2907),(1.45, 219.18)), ((2908, 2917),(19.6, 241.55)), ((2918, 2918),(-247.93, 52.87)),
((2919, 2919),(75.55, 95.35)), ((2920, 2920),(-268.09, 151.1)), ((2921, 2921),(117.16, 259.03)),
((2922, 2923),(-40.03, -16.7)), ((2924, 2933),(-202.29, 122.64)), ((2934, 2934),(128.31, 181.79)),
((2935, 2939),(-217.5, -14.38)), ((2940, 2940),(156.52, 337.59)), ((2941, 2950),(243.07, 293.45)),
((2951, 2951),(-168.0, 93.07)), ((2952, 2955),(230.32, 347.79)), ((2956, 2965),(119.03, 278.42)),
((2966, 2966),(-98.9, 63.56)), ((2967, 2967),(280.9, 296.72)), ((2968, 2977),(-214.76, -114.95)),
((2978, 2982),(44.34, 245.36)), ((2983, 2992),(-108.01, 336.89)), ((2995, 2997),(-168.96, -21.08)),
((2998, 2998),(237.05, 246.7)), ((2999, 2999),(146.41, 363.06)), ((3000, 3000),(108.55, 110.68)),
((3001, 3001),(298.17, 358.98)), ((3002, 3007),(-106.52, 187.52)), ((3008, 3009),(191.74, 233.14)),
((3010, 3017),(10.15, 346.2)), ((3018, 3018),(-198.95, 377.64)), ((3019, 3019),(184.33, 369.67)),
((3020, 3020),(-97.05, 34.89)), ((3021, 3022),(-109.87, 236.77)), ((3023, 3023),(-23.61, 373.6)),
((3024, 3025),(-182.82, -37.33)), ((3026, 3035),(-191.07, 285.18)), ((3036, 3039),(-173.54, 384.16)),
((3040, 3045),(-269.43, -151.11)), ((3046, 3046),(53.72, 305.96)), ((3047, 3048),(-223.02, -176.81)),
((3049, 3049),(103.09, 255.11)), ((3050, 3059),(-65.53, 74.93)), ((3060, 3064),(-241.05, -66.31)),
((3065, 3066),(-98.7, 42.07)), ((3067, 3067),(-168.99, 375.39)), ((3068, 3071),(-240.7, 364.46)),
((3073, 3073),(-47.72, 175.82)), ((3074, 3075),(-48.31, 80.15)), ((3076, 3079),(303.02, 356.8)),
((3080, 3080),(28.68, 114.78)), ((3081, 3081),(-83.72, 139.91)), ((3082, 3091),(-212.52, 335.85)),
((3092, 3097),(56.22, 371.84)), ((3098, 3100),(234.04, 288.17)), ((3101, 3106),(-148.51, 8.15)),
((3107, 3107),(-203.75, 4.53)), ((3108, 3108),(-172.81, 314.53)), ((3109, 3118),(-24.0, 92.9)),
((3119, 3119),(212.81, 374.09)), ((3120, 3120),(112.1, 338.09)), ((3121, 3130),(-172.87, 50.38)),
((3131, 3131),(-50.87, 28.65)), ((3132, 3132),(-102.09, 6.05)), ((3133, 3142),(-269.42, 259.93)),
((3143, 3144),(-178.27, 0.72)), ((3145, 3150),(-87.95, 130.02)), ((3151, 3151),(-179.31, 120.98)),
((3152, 3161),(-197.98, -196.42)), ((3162, 3171),(141.41, 362.2)), ((3172, 3175),(-267.37, 296.04)),
((3176, 3185),(-186.59, 137.17)), ((3186, 3186),(64.86, 75.85)), ((3187, 3187),(-184.23, -1.99)),
((3188, 3193),(-178.24, -28.94)), ((3194, 3194),(215.85, 358.74)), ((3195, 3202),(-195.79, -56.48)),
((3203, 3212),(-127.74, 374.93)), ((3217, 3219),(-120.26, 7.59)), ((3220, 3220),(-244.55, -52.52)),
((3221, 3226),(30.07, 373.9)), ((3227, 3229),(-112.07, 199.7)), ((3230, 3233),(-72.46, 365.24)),
((3234, 3237),(-234.91, 246.86)), ((3239, 3239),(-228.16, 91.44)), ((3242, 3246),(185.29, 320.15)),
((3247, 3248),(-220.25, 72.63)), ((3249, 3251),(-267.8, -234.31)), ((3252, 3261),(-199.81, 316.61)),
((3262, 3262),(-0.52, 277.46)), ((3263, 3263),(250.62, 254.17)), ((3264, 3265),(-228.92, 70.28)),
((3266, 3266),(257.48, 282.92)), ((3267, 3269),(-186.3, -22.2)), ((3270, 3279),(-176.87, -30.5)),
((3280, 3287),(-220.38, 8.06)), ((3288, 3288),(-232.73, 356.14)), ((3289, 3290),(51.81, 376.03)),
((3291, 3291),(-267.03, 227.2)), ((3292, 3301),(-164.08, 189.23)), ((3302, 3306),(91.62, 197.78)),
((3307, 3309),(190.53, 304.78)), ((3310, 3317),(-235.88, -86.44)), ((3318, 3318),(-235.34, 190.04)),
((3319, 3328),(268.47, 345.81)), ((3329, 3330),(-238.96, 331.05)), ((3331, 3332),(219.35, 330.94)),
((3333, 3333),(164.86, 179.26)), ((3334, 3334),(-153.03, -118.75)), ((3335, 3336),(158.05, 290.07)),
((3337, 3340),(-10.34, 95.79)), ((3341, 3341),(-178.5, 147.96)), ((3342, 3344),(28.08, 29.78)),
((3345, 3349),(-198.76, 354.52)), ((3350, 3355),(-136.02, 173.77)), ((3356, 3356),(-238.92, 104.19)),
((3357, 3357),(-44.36, 324.22)), ((3358, 3367),(-265.7, -135.19)), ((3368, 3368),(-225.49, -97.33)),
((3369, 3369),(16.65, 172.88)), ((3370, 3370),(16.11, 220.67)), ((3372, 3372),(-86.2, -37.54)),
((3373, 3373),(-147.25, 97.3)), ((3374, 3379),(0.25, 318.49)), ((3380, 3380),(-244.87, 3.55)),
((3381, 3385),(-205.31, -39.19)), ((3386, 3389),(58.84, 147.02)), ((3390, 3399),(-36.89, 114.26)),
((3400, 3400),(20.06, 236.38)), ((3401, 3403),(-220.78, 354.95)), ((3404, 3404),(249.56, 367.85)),
((3405, 3414),(36.39, 346.04)), ((3415, 3415),(192.41, 259.42)), ((3416, 3425),(-162.89, -33.95)),
((3426, 3426),(28.46, 253.1)), ((3427, 3436),(72.97, 94.98)), ((3447, 3453),(-58.92, 248.17)),
((3455, 3464),(16.24, 178.0)), ((3465, 3470),(-5.02, 220.74)), ((3471, 3480),(181.47, 267.0)),
((3481, 3481),(109.68, 146.74)), ((3482, 3482),(-98.05, 316.71)), ((3483, 3484),(102.46, 243.37)),
((3485, 3486),(-184.04, 16.99)), ((3487, 3496),(98.27, 99.18)), ((3498, 3498),(-52.57, 356.6)),
((3502, 3511),(-261.41, 267.83)), ((3512, 3513),(-154.53, -118.25)), ((3514, 3514),(9.25, 170.78)),
((3515, 3524),(-196.17, -58.36)), ((3525, 3526),(-155.65, -58.55)), ((3527, 3534),(-140.51, 269.47)),
((3535, 3535),(-118.9, -11.57)), ((3536, 3537),(-76.82, -45.28)), ((3538, 3544),(273.55, 360.51)),
((3545, 3545),(-104.78, -94.19)), ((3547, 3553),(-153.07, 186.34)), ((3554, 3554),(-77.58, 284.05)),
((3555, 3564),(-99.08, 66.84)), ((3565, 3565),(-198.45, 259.51)), ((3566, 3566),(-148.28, 88.7)),
((3567, 3570),(226.56, 371.39)), ((3571, 3580),(-0.28, 339.5)), ((3581, 3582),(-108.45, 368.11)),
((3584, 3586),(-52.71, 166.51)), ((3587, 3587),(-251.95, -125.06)), ((3588, 3588),(32.89, 228.0)),
((3589, 3589),(-178.17, -34.72)), ((3590, 3590),(-160.68, 379.62)), ((3598, 3607),(-124.64, -76.67)),
((3608, 3609),(-208.29, 166.13)), ((3610, 3612),(-214.79, 16.07)), ((3613, 3613),(312.13, 343.84)),
((3614, 3614),(-82.32, 40.44)), ((3615, 3624),(98.74, 121.78)), ((3630, 3639),(-224.95, 214.9)),
((3734, 3743),(-5.56, 155.98)), ((3744, 3748),(48.73, 90.62)), ((3749, 3753),(135.65, 281.97)),
((3754, 3756),(-208.96, 283.97)), ((3757, 3766),(-111.99, 71.94)), ((3783, 3788),(63.8, 318.67)),
((3803, 3804),(-19.15, 16.02)), ((3805, 3814),(-88.26, 47.16)), ((3853, 3860),(122.13, 138.07)),
((3894, 3903),(252.05, 291.18)), ((3953, 3961),(-130.97, 340.64)), ((3963, 3972),(23.81, 122.9)),
((3980, 3984),(-42.37, 273.27)), ((3986, 3987),(-4.38, 132.49)), ((3988, 3997),(83.23, 351.73)),
((4098, 4105),(-208.8, 274.32)), ((4106, 4108),(-81.6, 259.65)), ((4109, 4118),(-94.97, 10.57)),
((4119, 4119),(213.36, 274.56)), ((4120, 4121),(-192.44, 361.1)), ((4122, 4122),(103.02, 190.25)),
((4123, 4123),(-238.08, -84.97)), ((4124, 4133),(-264.29, -258.26)), ((4134, 4138),(-24.3, 46.18)),
((4139, 4148),(-23.66, 326.13)), ((4149, 4151),(27.33, 371.84)), ((4152, 4161),(-12.32, 245.72)),
((4162, 4164),(22.09, 94.33)), ((4165, 4168),(-33.09, 340.98)), ((4171, 4172),(-182.82, 171.77)),
((4173, 4177),(-171.6, 116.89)), ((4181, 4190),(-193.95, -28.16)), ((4193, 4194),(196.41, 285.73)),
((4195, 4201),(-171.55, 156.22)), ((4207, 4209),(-21.5, 323.9)), ((4210, 4210),(-27.88, 170.53)),
((4212, 4221),(136.15, 365.27)), ((4222, 4224),(-161.11, 385.04)), ((4227, 4228),(83.68, 258.79)),
((4229, 4229),(-25.74, 96.01)), ((4230, 4230),(172.08, 301.89)), ((4231, 4236),(-32.37, -12.75)),
((4237, 4246),(-251.81, 308.78)), ((4276, 4285),(-151.7, 257.49)), ((4326, 4335),(161.49, 220.96)),
((4341, 4341),(183.38, 264.2)), ((4342, 4351),(-148.14, 77.94)), ((4391, 4400),(-132.35, 114.12)),
((4428, 4437),(274.67, 320.54)), ((4441, 4450),(-171.4, -80.94)), ((4523, 4532),(-38.76, 122.55)),
((4623, 4624),(-198.86, 64.17)), ((4681, 4690),(-76.28, 186.99)), ((4691, 4692),(9.93, 258.19)),
((4693, 4694),(-95.03, 231.85)), ((4695, 4695),(108.78, 241.79)), ((4696, 4696),(17.61, 20.31)),
((4697, 4706),(-71.7, 109.77)), ((4718, 4719),(-138.14, 219.04)), ((4720, 4720),(-240.28, 166.55)),
((4721, 4730),(-97.43, -3.54)), ((4731, 4738),(-54.92, 190.85)), ((4739, 4739),(213.51, 263.87)),
((4740, 4743),(-253.4, 86.62)), ((4744, 4744),(-162.05, -63.89)), ((4745, 4745),(-77.08, 353.85)),
((4746, 4755),(5.64, 141.19)), ((4757, 4758),(-252.16, 253.91)), ((4759, 4759),(-55.06, 230.06)),
((4760, 4760),(-54.93, 109.32)), ((4763, 4763),(116.23, 218.38)), ((4764, 4768),(-189.38, 48.24)),
((4769, 4769),(-181.16, 136.07)), ((4770, 4770),(15.07, 27.98)), ((4771, 4780),(-71.01, 277.77)),
((4781, 4783),(-118.87, 44.57)), ((4784, 4784),(-115.78, 319.73)), ((4785, 4785),(-140.02, -2.24)),
((4786, 4786),(-116.01, 166.61)), ((4787, 4796),(-125.16, 256.47)), ((4797, 4806),(-262.32, 144.37)),
((4807, 4808),(-177.87, -0.98)), ((4809, 4810),(-162.43, 173.59)), ((4811, 4811),(28.54, 117.37)),
((4812, 4818),(62.66, 263.97)), ((4819, 4820),(-216.92, -130.03)), ((4821, 4824),(328.63, 358.76)),
((4827, 4836),(-54.04, 260.38)), ((4837, 4837),(-110.75, 65.05)), ((4838, 4839),(0.42, 292.01)),
((4840, 4846),(-214.28, 361.15)), ((4847, 4847),(-103.46, 223.96)), ((4848, 4857),(145.54, 267.42)),
((4858, 4858),(-145.36, 97.73)), ((4859, 4859),(-219.61, 115.16)), ((4860, 4866),(-91.32, 43.86)),
((4867, 4868),(-66.4, 108.18)), ((4871, 4871),(-207.86, 301.95)), ((4872, 4877),(258.8, 324.89)),
((4878, 4887),(91.28, 136.39)), ((4888, 4888),(215.74, 303.95)), ((4889, 4889),(-221.3, -161.6)),
((4890, 4893),(-233.84, -135.23)), ((4897, 4906),(89.7, 160.08)), ((4907, 4916),(-218.88, 356.23)),
((4917, 4917),(25.85, 331.79)), ((4931, 4940),(-140.32, 117.73)), ((5622, 5629),(126.16, 281.64)),
((5630, 5639),(-172.59, 134.3)), ((5718, 5725),(288.25, 295.43)), ((5826, 5835),(249.83, 259.65)),
((5864, 5873),(218.72, 319.1)), ((6116, 6125),(-37.93, 373.64)), ((6155, 6161),(-258.16, 362.24)),
((6162, 6166),(14.34, 285.0)), ((6169, 6178),(-172.07, 38.28)), ((6184, 6193),(-239.99, 83.52)),
((6233, 6242),(-112.56, 77.47)), ((6309, 6318),(215.65, 318.6)), ((6319, 6319),(-95.14, 188.89)),
((6323, 6327),(-59.05, 93.72)), ((6346, 6355),(11.43, 225.69)), ((6404, 6413),(177.32, 253.1)),
((6445, 6454),(-117.39, -47.83)), ((6810, 6819),(-233.91, 309.35)), ((6993, 7002),(-244.99, -145.88)),
((7899, 7908),(-148.8, 121.21)), ((7921, 7923),(-147.04, 176.22)), ((7925, 7925),(-87.36, 381.61)),
((7927, 7934),(120.36, 330.39)), ((7940, 7949),(254.3, 298.63)), ((7950, 7955),(78.69, 267.18)),
((7956, 7956),(-259.28, -6.44)), ((7957, 7966),(-146.25, -77.53)), ((7998, 8007),(-243.09, 30.38)),
((8020, 8024),(0.03, 96.28)), ((8031, 8040),(-109.86, 270.55)), ((8041, 8041),(-64.98, 383.13)),
((8043, 8044),(-166.18, 126.18)), ((8045, 8046),(-170.07, 120.96)), ((8048, 8050),(196.66, 316.19)),
((8052, 8061),(103.14, 169.8)), ((8122, 8131),(10.91, 53.63)), ((8132, 8137),(107.72, 384.35)),
((8138, 8147),(136.73, 147.96)), ((8150, 8151),(61.93, 267.73)), ((8152, 8161),(-104.54, 228.53)),
((8162, 8164),(297.27, 369.15)), ((8165, 8165),(-85.1, 323.45)), ((8166, 8169),(159.65, 367.31)),
((8170, 8170),(-115.77, 286.14)), ((8171, 8175),(-77.23, 11.82)), ((8176, 8177),(-16.92, 260.59)),
((8178, 8187),(-198.31, -173.23)), ((8188, 8188),(-48.89, 206.16)), ((8189, 8189),(-78.99, 227.06)),
((8190, 8191),(-267.02, -49.33)), ((8193, 8202),(-141.49, 185.82)), ((8203, 8205),(36.9, 107.72)),
((8206, 8215),(-153.44, -21.43)), ((8216, 8217),(-221.05, 24.22)), ((8218, 8227),(-60.36, 24.52)),
((8281, 8290),(-208.9, -197.29)), ((8305, 8314),(228.45, 323.63)), ((8400, 8409),(-196.66, 338.1)),
((8475, 8484),(-237.64, -127.49)), ((8503, 8512),(-24.86, 163.03)), ((8514, 8515),(-144.18, 332.96)),
((8516, 8523),(-155.08, 90.85)), ((8525, 8531),(-59.83, -32.03)), ((8534, 8543),(-31.82, 69.6)),
((8544, 8544),(26.36, 35.67)), ((8545, 8548),(-87.26, 228.8)), ((8549, 8549),(-213.1, 27.43)),
((8550, 8550),(151.68, 311.33)), ((8551, 8551),(-241.31, -82.99)), ((8552, 8552),(237.72, 238.38)),
((8553, 8562),(-255.76, -94.72)), ((8563, 8565),(119.42, 299.47)), ((8566, 8569),(-241.22, -60.6)),
((8570, 8570),(141.86, 269.42)), ((8571, 8579),(233.09, 251.52)), ((8580, 8580),(-118.94, 261.36)),
((8581, 8584),(-149.43, -147.63)), ((8585, 8594),(159.66, 356.38)), ((8595, 8595),(59.34, 191.55)),
((8596, 8596),(-212.7, -70.53)), ((8597, 8605),(-135.07, -0.31)), ((8606, 8606),(-137.91, -97.37)),
((8607, 8616),(-196.78, 29.94)), ((8617, 8626),(-231.55, -105.22)), ((8627, 8627),(-215.06, -140.14)),
((8628, 8629),(-96.12, 161.01)), ((8630, 8636),(148.55, 378.16)), ((8637, 8646),(116.83, 336.65)),
((8647, 8649),(-206.57, 144.79)), ((8650, 8652),(-0.76, 73.3)), ((8653, 8654),(-71.24, 161.62)),
((8655, 8655),(-154.1, 101.27)), ((8656, 8665),(167.23, 174.25)), ((8666, 8667),(-135.79, 9.56)),
((8668, 8677),(-268.28, -201.36)), ((8678, 8680),(-72.61, 337.68)), ((8681, 8682),(122.15, 140.68)),
((8683, 8687),(-175.56, 366.63)), ((8688, 8688),(154.43, 282.44)), ((8689, 8689),(-116.29, 16.21)),
((8690, 8690),(85.72, 88.47)), ((8691, 8700),(-189.47, -150.88)), ((8701, 8701),(120.55, 215.16)),
((8702, 8708),(334.14, 379.29)), ((8709, 8709),(-18.13, 350.54)), ((8710, 8717),(-61.12, 89.49)),
((8718, 8719),(-169.74, -55.66)), ((8720, 8721),(-72.88, 228.07)), ((8722, 8723),(-251.75, -94.66)),
((8724, 8724),(-173.93, 259.83)), ((8725, 8728),(208.99, 351.82)), ((8729, 8738),(-189.74, -129.94)),
((8739, 8740),(156.52, 202.9)), ((8741, 8742),(172.25, 182.61)), ((8743, 8744),(-50.28, 275.56)),
((8745, 8746),(-74.2, 320.11)), ((8747, 8756),(-66.97, -32.55)), ((8757, 8757),(-138.62, 227.63)),
((8758, 8758),(218.6, 333.05)), ((8759, 8768),(-111.6, 239.02)), ((8769, 8771),(2.9, 338.95)),
((8772, 8774),(-263.25, 324.63)), ((8775, 8775),(-47.08, 39.89)), ((8776, 8781),(-107.56, 27.97)),
((8782, 8783),(-156.88, 8.96)), ((8784, 8784),(-84.61, 364.94)), ((8785, 8785),(1.84, 65.13)),
((8786, 8786),(-75.13, 247.11)), ((8787, 8787),(342.08, 368.3)), ((8788, 8797),(281.62, 290.66)),
((8798, 8799),(-162.66, -34.59)), ((8800, 8800),(-166.07, 293.91)), ((8801, 8803),(-267.07, -106.69)),
((8804, 8810),(248.94, 384.3)), ((8811, 8812),(-24.22, 307.31)), ((8813, 8822),(-237.08, 328.01)),
((8825, 8834),(-97.01, -30.18)), ((8835, 8835),(284.25, 361.78)), ((8838, 8847),(-91.34, 1.86)),
((8882, 8891),(-71.69, 186.24)), ((8893, 8894),(-268.7, -0.01)), ((8895, 8898),(-103.85, 179.59)),
((8905, 8914),(-43.29, 281.38)), ((8915, 8916),(-53.51, -13.68)), ((8917, 8917),(-209.1, 161.36)),
((8918, 8925),(65.58, 183.73)), ((8926, 8928),(183.98, 382.99)), ((8929, 8929),(-107.91, 1.59)),
((8930, 8930),(90.94, 252.32)), ((8931, 8931),(-25.31, 244.37)), ((8932, 8932),(-252.04, -62.55)),
((8933, 8942),(32.94, 315.29)), ((8943, 8943),(14.08, 376.81)), ((8944, 8953),(67.48, 378.51)),
((8954, 8954),(-228.9, -122.57)), ((8955, 8958),(-138.1, 137.37)), ((8959, 8961),(219.6, 364.83)),
((8962, 8962),(-86.83, -15.0)), ((8963, 8963),(-74.91, 98.19)), ((8964, 8964),(-204.06, 179.34)),
((8965, 8965),(-201.28, 72.17)), ((8966, 8966),(-257.44, 67.0)), ((8967, 8970),(-248.95, -99.83)),
((8971, 8980),(-131.0, 211.12)), ((8981, 8982),(4.52, 97.01)), ((8983, 8983),(-132.27, -117.69)),
((8984, 8993),(-199.99, 186.49)), ((8997, 8997),(-4.1, 145.44)), ((9000, 9001),(225.34, 363.34)),
((9002, 9002),(60.99, 331.71)), ((9003, 9003),(-265.7, 348.77)), ((9008, 9017),(-26.46, 164.63)),
((9028, 9029),(-34.89, -18.24)), ((9030, 9030),(-88.18, 351.66)), ((9031, 9031),(-114.25, 282.37)),
((9032, 9041),(-248.8, 235.37)), ((9103, 9105),(-249.52, -150.77)), ((9115, 9124),(117.25, 324.39)),
((9125, 9126),(-174.78, 367.95)), ((9127, 9130),(-237.61, -146.09)), ((9134, 9142),(276.67, 372.24)),
((9143, 9143),(-226.68, -105.86)), ((9144, 9144),(-142.96, 278.26)), ((9145, 9145),(-149.27, 335.24)),
((9146, 9155),(-139.14, -86.97)), ((9157, 9158),(128.83, 374.48)), ((9159, 9160),(43.96, 226.1)),
((9161, 9170),(-188.55, 221.15)), ((9171, 9177),(119.74, 161.65)), ((9178, 9178),(57.4, 268.75)),
((9179, 9183),(-153.0, 132.0)), ((9184, 9193),(54.83, 332.06)), ((9199, 9201),(-208.9, 99.13)),
((9226, 9235),(-153.17, 145.99)), ((9236, 9236),(-180.86, 205.59)), ((9237, 9238),(279.39, 379.35)),
((9239, 9241),(-29.78, 96.3)), ((9242, 9242),(28.64, 363.36)), ((9243, 9252),(-87.92, -55.45)),
((9253, 9254),(-225.25, -79.98)), ((9255, 9259),(-101.64, -100.49)), ((9260, 9260),(152.01, 183.62)),
((9261, 9270),(-3.38, 335.55)), ((9271, 9273),(152.91, 354.95)), ((9274, 9275),(-136.05, 360.05)),
((9276, 9285),(-210.98, 239.0)), ((9286, 9294),(-246.66, 120.63)), ((9295, 9295),(-56.87, 54.69)),
((9296, 9297),(-176.74, 40.4)), ((9300, 9300),(-54.97, 237.49)), ((9301, 9307),(-254.71, 123.63)),
((9309, 9316),(-268.92, 184.45)), ((9317, 9318),(-243.37, -138.17)), ((9319, 9319),(-221.38, 65.71)),
((9320, 9329),(-116.63, 124.41)), ((9330, 9330),(-123.74, -104.73)), ((9331, 9334),(-162.19, 126.98)),
((9335, 9337),(-124.13, 349.08)), ((9338, 9347),(-185.56, 307.33)), ((9348, 9348),(198.81, 248.59)),
((9349, 9349),(76.78, 361.42)), ((9357, 9365),(-152.93, -140.32)), ((9368, 9377),(-204.67, 328.06)),
((9383, 9383),(-246.58, 0.34)), ((9387, 9392),(-32.55, 122.65)), ((9401, 9403),(-196.37, 14.23)),
((9404, 9404),(-156.96, 61.01)), ((9405, 9407),(-234.92, 104.49)), ((9411, 9411),(-84.25, 46.51)),
((9412, 9416),(225.56, 330.93)), ((9417, 9417),(-76.83, 140.99)), ((9418, 9427),(-77.14, 10.0)),
((9462, 9471),(-166.76, -117.43)), ((9472, 9481),(-119.88, -51.58)), ((9482, 9482),(-250.4, 248.14)),
((9483, 9483),(-150.61, -5.87)), ((9484, 9492),(-261.12, -195.7)), ((9493, 9493),(-197.0, -195.2)),
((9494, 9494),(-229.64, 368.62)), ((9495, 9495),(-111.76, 295.09)), ((9496, 9496),(217.14, 282.57)),
((9497, 9501),(-96.58, 288.98)), ((9502, 9511),(109.8, 274.21)), ((9512, 9513),(-235.37, 309.41)),
((9514, 9516),(-81.82, 299.23)), ((9517, 9517),(-156.84, 98.12)), ((9518, 9527),(-57.14, 193.5)),
((9528, 9528),(-211.19, 235.01)), ((9529, 9530),(66.82, 117.38)), ((9531, 9534),(27.46, 146.94)),
((9535, 9536),(194.64, 337.88)), ((9537, 9546),(-130.13, 182.26)), ((9547, 9548),(-249.9, 144.54)),
((9549, 9558),(96.08, 312.42)), ((9559, 9560),(-44.31, 188.99)), ((9561, 9561),(-120.93, -59.19)),
((9562, 9562),(-29.07, -3.58)), ((9563, 9572),(-120.48, 265.31)), ((9573, 9574),(-214.94, 275.46)),
((9575, 9584),(-57.55, 274.07)), ((9585, 9586),(8.5, 209.77)), ((9587, 9594),(-217.53, 232.26)),
((9595, 9597),(-99.7, 317.61)), ((9598, 9598),(18.57, 92.64)), ((9599, 9608),(-123.79, 286.26)),
((9609, 9609),(21.56, 205.47)), ((9610, 9614),(210.18, 291.39)), ((9615, 9616),(24.53, 80.75)),
((9617, 9626),(216.09, 254.99)), ((9627, 9628),(-224.76, 262.2)), ((9629, 9638),(-137.57, 259.18)),
((9640, 9642),(-268.52, 290.92)), ((9644, 9653),(-260.64, -244.64)), ((9654, 9660),(-257.5, -95.02)),
((9661, 9670),(-36.0, 204.74)), ((9671, 9671),(-5.29, 155.7)), ((9672, 9674),(-242.24, -2.71)),
((9675, 9675),(197.7, 273.36)), ((9676, 9678),(-74.84, 347.25)), ((9679, 9679),(-49.11, 93.21)),
((9680, 9680),(-236.26, 220.2)), ((9681, 9690),(251.25, 367.53)), ((9691, 9693),(-235.0, -166.08)),
((9694, 9702),(-132.7, 202.73)), ((9703, 9703),(306.81, 370.65)), ((9704, 9713),(-207.9, 52.89)),
((9714, 9715),(-200.01, 41.6)), ((9716, 9717),(-261.37, -63.28)), ((9718, 9727),(-237.88, 302.0)),
((9729, 9730),(4.73, 16.08)), ((9734, 9743),(-177.74, 55.62)), ((9746, 9750),(-26.62, 204.07)),
((9751, 9751),(112.05, 205.38)), ((9752, 9752),(-136.32, 182.19)), ((9753, 9755),(1.44, 126.37)),
((9764, 9765),(45.71, 269.52)), ((9766, 9766),(-238.25, -117.52)), ((9767, 9776),(-172.05, 200.86)),
((9777, 9778),(-46.71, 283.7)), ((9780, 9784),(-264.84, -113.38)), ((9785, 9794),(37.38, 115.26)),
((9800, 9801),(275.69, 299.24)), ((9805, 9806),(-52.39, 190.83)), ((9810, 9819),(-218.0, 316.57)),
((9867, 9876),(-59.41, -44.5)), ((9880, 9880),(-255.05, 123.81)), ((9887, 9896),(140.59, 306.77)),
((9917, 9918),(-132.8, 162.31)), ((9919, 9920),(-200.89, -1.07)), ((9921, 9921),(-157.79, 12.0)),
((9922, 9924),(-231.52, -53.09)), ((9925, 9931),(-211.82, 172.15)), ((9932, 9932),(-243.9, 95.85)),
((9933, 9939),(9.4, 375.82)), ((9940, 9940),(-74.06, 94.46)), ((9941, 9941),(-182.92, 292.26)),
((9942, 9944),(84.75, 98.82)), ((9945, 9949),(19.67, 25.4)), ((9950, 9959),(11.31, 114.89)),
((9961, 9961),(-73.74, 6.88)), ((9962, 9962),(-205.37, -82.2)), ((9963, 9966),(-1.83, 260.74)),
((9978, 9982),(-185.8, -31.75)), ((9983, 9992),(39.55, 243.89)), ((9993, 9997),(202.23, 314.81)),
((9998, 9999),(231.95, 366.76))))
	)

        x.fold(y)((acc: Real, xi: Real) => {0.75 * xi - 0.125 * acc})
    }


}