import daisy.lang._
import Real._
import daisy.lang.Vector._

object stdDeviation {

	def stdDeviation(x: Vector): Real = {
require(x >= -160.06 && x <= 360.98 && x.size(10000)
	 && x.specV(Set(((0, 2),(121.35, 153.28)), ((3, 12),(88.77, 342.5)), ((13, 13),(162.03, 300.35)),
((14, 15),(109.67, 217.24)), ((16, 16),(3.23, 129.32)), ((17, 17),(156.55, 172.56)),
((18, 27),(-29.96, 31.48)), ((28, 29),(-9.33, -1.53)), ((30, 38),(95.33, 128.28)),
((39, 39),(124.4, 193.93)), ((40, 42),(-156.98, 248.03)), ((43, 44),(-19.37, 267.55)),
((45, 45),(191.87, 243.45)), ((46, 46),(294.86, 358.47)), ((47, 56),(7.68, 317.82)),
((57, 60),(-136.74, 262.03)), ((61, 63),(76.68, 334.72)), ((64, 73),(-69.31, 284.9)),
((74, 75),(247.93, 348.95)), ((76, 76),(-94.48, -51.94)), ((77, 79),(186.68, 327.19)),
((80, 81),(175.0, 203.43)), ((82, 82),(-131.76, 2.14)), ((83, 83),(319.25, 335.6)),
((84, 85),(48.39, 232.92)), ((86, 86),(-146.13, 242.08)), ((87, 96),(-159.24, -42.09)),
((97, 97),(61.95, 322.49)), ((98, 107),(-94.12, 6.27)), ((108, 108),(-31.59, 300.94)),
((109, 113),(110.73, 316.55)), ((114, 123),(133.58, 161.86)), ((124, 127),(24.4, 46.1)),
((128, 128),(40.99, 298.57)), ((129, 130),(-80.17, -43.02)), ((131, 140),(-8.19, 336.13)),
((141, 143),(-59.77, 57.72)), ((144, 144),(7.79, 341.38)), ((145, 146),(266.5, 354.45)),
((147, 149),(190.78, 313.78)), ((150, 152),(2.97, 98.47)), ((153, 153),(-15.91, 134.92)),
((154, 155),(173.36, 280.41)), ((156, 165),(-144.46, 347.26)), ((166, 166),(274.54, 295.28)),
((167, 167),(12.29, 198.83)), ((168, 168),(117.56, 230.52)), ((169, 172),(-48.27, 137.27)),
((173, 179),(-115.38, 73.39)), ((180, 184),(1.7, 329.8)), ((186, 188),(321.5, 330.26)),
((189, 189),(134.87, 210.94)), ((190, 199),(-106.77, 275.36)), ((200, 200),(144.46, 153.52)),
((201, 206),(19.18, 129.98)), ((221, 230),(-134.45, 123.73)), ((231, 234),(-148.49, 276.19)),
((235, 244),(-76.63, 343.61)), ((245, 251),(-59.42, 279.34)), ((252, 252),(161.94, 301.11)),
((253, 255),(-154.22, 1.88)), ((256, 265),(-26.37, 319.27)), ((266, 272),(105.4, 288.28)),
((273, 282),(-48.18, 177.24)), ((288, 297),(147.55, 238.1)), ((298, 300),(36.97, 254.75)),
((301, 302),(-68.5, 150.34)), ((303, 303),(-101.76, -60.79)), ((304, 304),(-147.41, 148.14)),
((306, 306),(335.12, 358.66)), ((307, 313),(-83.82, 275.53)), ((314, 314),(-12.38, -2.75)),
((316, 319),(-32.21, 155.65)), ((320, 320),(-95.91, 80.55)), ((322, 323),(240.32, 315.65)),
((324, 325),(90.0, 154.75)), ((332, 334),(-32.63, 134.61)), ((335, 344),(-9.75, -2.42)),
((345, 346),(-104.01, 92.04)), ((347, 347),(171.66, 311.79)), ((349, 357),(10.43, 286.07)),
((358, 362),(-17.8, 327.01)), ((363, 363),(-88.1, 38.23)), ((364, 365),(78.95, 252.48)),
((366, 375),(153.57, 236.25)), ((383, 384),(-76.83, -24.98)), ((385, 385),(-95.79, 325.82)),
((386, 386),(224.19, 358.82)), ((387, 387),(32.75, 315.49)), ((388, 388),(62.15, 111.81)),
((389, 398),(63.09, 294.55)), ((412, 414),(45.12, 239.68)), ((416, 425),(-58.22, 280.33)),
((426, 427),(31.5, 320.93)), ((428, 437),(122.59, 187.4)), ((438, 438),(36.67, 126.89)),
((439, 442),(89.58, 191.92)), ((443, 443),(58.91, 349.51)), ((444, 453),(114.28, 153.38)),
((454, 454),(172.49, 359.61)), ((455, 459),(-140.99, -52.3)), ((463, 467),(92.15, 324.61)),
((471, 480),(-0.35, 237.66)), ((481, 481),(68.58, 295.4)), ((482, 482),(-110.13, -93.54)),
((483, 483),(-38.9, 292.88)), ((484, 486),(55.33, 186.87)), ((488, 495),(192.72, 285.84)),
((496, 505),(74.64, 213.18)), ((550, 559),(-86.94, 75.54)), ((561, 566),(-69.72, 102.76)),
((568, 568),(288.89, 333.11)), ((569, 573),(-132.42, -58.46)), ((574, 578),(219.85, 238.2)),
((584, 593),(-65.25, -64.07)), ((598, 598),(-129.12, 292.58)), ((608, 610),(-61.83, 122.03)),
((611, 615),(112.35, 253.43)), ((632, 641),(-148.27, 19.01)), ((724, 733),(-60.28, 180.44)),
((734, 734),(-140.91, -51.66)), ((735, 744),(72.19, 126.23)), ((745, 747),(19.02, 210.84)),
((749, 752),(173.68, 353.01)), ((753, 754),(119.74, 213.79)), ((762, 771),(-121.53, -116.62)),
((781, 787),(-6.44, 188.99)), ((790, 799),(99.66, 107.48)), ((803, 803),(73.65, 204.78)),
((804, 804),(81.74, 269.74)), ((805, 808),(49.71, 229.56)), ((809, 809),(-22.63, 146.28)),
((811, 811),(42.27, 174.91)), ((812, 813),(80.66, 337.59)), ((816, 818),(60.3, 177.96)),
((819, 828),(-76.37, 340.15)), ((939, 948),(203.56, 233.29)), ((970, 974),(116.11, 260.85)),
((990, 999),(-88.37, 216.83)), ((1050, 1059),(-7.92, 195.26)), ((1063, 1063),(-35.68, 133.89)),
((1064, 1073),(-28.65, 142.25)), ((1090, 1099),(-123.95, -88.6)), ((1234, 1243),(95.74, 112.45)),
((1245, 1245),(64.0, 135.3)), ((1246, 1248),(141.06, 355.95)), ((1249, 1249),(-6.68, 359.18)),
((1250, 1251),(201.37, 324.25)), ((1252, 1261),(16.46, 244.33)), ((1262, 1266),(-142.36, 311.18)),
((1272, 1281),(-129.54, 149.32)), ((1291, 1300),(12.33, 262.73)), ((1389, 1398),(-98.21, 338.96)),
((1399, 1402),(53.27, 186.23)), ((1403, 1407),(13.69, 134.99)), ((1408, 1408),(-78.4, -62.94)),
((1409, 1417),(101.97, 252.24)), ((1418, 1418),(227.68, 254.64)), ((1419, 1419),(160.62, 183.42)),
((1420, 1423),(-69.77, -28.71)), ((1424, 1424),(127.74, 311.8)), ((1425, 1428),(167.58, 195.84)),
((1429, 1429),(13.49, 88.37)), ((1430, 1430),(-56.6, 272.7)), ((1431, 1431),(-89.71, 252.5)),
((1432, 1441),(232.16, 237.87)), ((1442, 1442),(3.82, 271.71)), ((1443, 1443),(134.71, 233.89)),
((1444, 1444),(297.34, 342.77)), ((1445, 1454),(-2.44, 181.75)), ((1455, 1456),(-87.5, 239.17)),
((1457, 1457),(-115.68, 314.99)), ((1458, 1461),(-64.79, 61.22)), ((1465, 1466),(264.73, 308.53)),
((1469, 1471),(87.77, 325.67)), ((1472, 1476),(176.31, 335.41)), ((1479, 1486),(-142.88, 355.18)),
((1487, 1487),(-97.19, 187.74)), ((1488, 1488),(-112.67, 90.47)), ((1489, 1496),(-94.19, 78.37)),
((1497, 1506),(263.16, 332.62)), ((1507, 1510),(105.41, 336.59)), ((1511, 1512),(-8.48, 127.33)),
((1513, 1516),(122.48, 354.3)), ((1517, 1518),(-76.65, 235.86)), ((1519, 1528),(-143.45, 178.42)),
((1529, 1531),(-122.65, 35.94)), ((1532, 1541),(-150.22, -126.42)), ((1575, 1584),(100.18, 251.68)),
((1585, 1587),(-93.26, 228.97)), ((1594, 1603),(213.57, 356.12)), ((1620, 1629),(-65.7, 132.72)),
((1635, 1640),(-66.28, 86.46)), ((1641, 1650),(-99.54, -69.41)), ((1651, 1651),(-42.13, 140.78)),
((1652, 1652),(-87.52, -81.78)), ((1653, 1654),(-13.29, 271.41)), ((1655, 1656),(168.85, 252.04)),
((1658, 1658),(-62.55, 338.93)), ((1659, 1660),(-8.87, 84.01)), ((1661, 1662),(-135.65, 357.84)),
((1663, 1666),(-41.63, 160.97)), ((1667, 1676),(-17.56, 104.97)), ((1677, 1677),(-51.63, 3.87)),
((1678, 1678),(162.98, 304.74)), ((1679, 1685),(273.13, 342.72)), ((1686, 1686),(-48.59, 109.21)),
((1687, 1696),(-153.11, -26.73)), ((1697, 1698),(-42.41, 333.57)), ((1699, 1708),(135.0, 231.06)),
((1709, 1710),(-103.42, -19.29)), ((1711, 1718),(-5.69, 92.28)), ((1719, 1720),(-134.76, 245.06)),
((1721, 1730),(-96.86, 211.83)), ((1731, 1732),(21.63, 258.15)), ((1733, 1733),(49.08, 121.04)),
((1734, 1743),(295.58, 355.71)), ((1834, 1843),(72.27, 120.07)), ((1844, 1844),(27.15, 55.93)),
((1845, 1845),(-125.26, -2.27)), ((1846, 1847),(-154.13, 321.08)), ((1848, 1854),(1.96, 291.33)),
((1855, 1864),(79.43, 236.13)), ((1865, 1866),(203.22, 360.64)), ((1867, 1871),(-88.7, 236.04)),
((1872, 1873),(-30.89, 288.38)), ((1874, 1876),(-61.59, 58.35)), ((1877, 1881),(-71.25, 335.78)),
((1882, 1891),(-149.12, 104.63)), ((1892, 1898),(-35.21, 38.23)), ((1899, 1900),(18.18, 29.5)),
((1901, 1901),(32.21, 168.63)), ((1902, 1906),(311.89, 340.0)), ((1907, 1907),(141.33, 323.46)),
((1908, 1910),(70.49, 90.51)), ((1911, 1911),(6.25, 288.37)), ((1912, 1912),(93.92, 104.81)),
((1913, 1913),(75.33, 172.35)), ((1914, 1923),(150.99, 239.9)), ((1924, 1931),(229.99, 247.71)),
((1936, 1945),(-125.02, 43.43)), ((1947, 1948),(35.07, 305.05)), ((1950, 1959),(223.43, 288.65)),
((1960, 1960),(110.65, 186.9)), ((1961, 1962),(146.66, 193.92)), ((1963, 1967),(-90.24, 273.15)),
((1968, 1970),(149.73, 312.61)), ((1971, 1980),(40.23, 51.82)), ((1981, 1981),(156.99, 326.81)),
((1982, 1991),(-39.74, 347.32)), ((1992, 1993),(204.99, 306.59)), ((1994, 2003),(-65.37, 205.0)),
((2004, 2007),(106.07, 276.92)), ((2008, 2008),(148.32, 215.92)), ((2009, 2009),(185.64, 198.58)),
((2010, 2010),(324.3, 324.5)), ((2012, 2013),(-107.94, 290.19)), ((2014, 2016),(-117.35, 267.0)),
((2017, 2021),(93.73, 130.42)), ((2022, 2022),(14.0, 149.87)), ((2023, 2023),(-94.71, 116.84)),
((2024, 2033),(-126.32, -32.09)), ((2034, 2043),(-146.6, 323.92)), ((2044, 2044),(-117.81, -93.5)),
((2045, 2045),(-112.76, 350.84)), ((2046, 2055),(98.91, 264.46)), ((2077, 2078),(9.32, 235.76)),
((2082, 2091),(-51.05, 305.08)), ((2092, 2092),(-35.15, -30.7)), ((2093, 2096),(-42.19, 214.98)),
((2097, 2100),(-106.38, 6.95)), ((2101, 2110),(-7.42, 287.01)), ((2111, 2115),(-137.3, 332.8)),
((2116, 2118),(139.56, 286.98)), ((2121, 2122),(-2.93, 73.56)), ((2127, 2127),(182.62, 235.87)),
((2128, 2136),(-144.39, -98.14)), ((2137, 2137),(-126.0, 259.71)), ((2138, 2138),(-2.2, 32.11)),
((2139, 2143),(8.21, 23.15)), ((2144, 2145),(-58.46, 60.42)), ((2146, 2155),(51.22, 181.0)),
((2156, 2157),(-37.08, 77.97)), ((2158, 2158),(-141.37, -125.05)), ((2159, 2160),(206.34, 215.16)),
((2161, 2162),(181.58, 235.63)), ((2163, 2166),(79.49, 295.29)), ((2167, 2167),(-62.67, 43.07)),
((2168, 2168),(-81.43, 251.69)), ((2169, 2170),(3.66, 55.51)), ((2171, 2172),(-115.31, -66.52)),
((2173, 2182),(41.34, 328.38)), ((2194, 2203),(-145.19, 303.26)), ((2204, 2207),(-137.16, -130.16)),
((2208, 2209),(105.74, 286.08)), ((2210, 2219),(-77.88, 227.12)), ((2220, 2220),(23.89, 139.35)),
((2221, 2222),(-41.55, 104.55)), ((2223, 2226),(77.17, 114.16)), ((2229, 2232),(-35.76, -32.64)),
((2233, 2234),(-141.73, 141.75)), ((2239, 2248),(69.25, 187.14)), ((2249, 2250),(54.57, 70.43)),
((2251, 2259),(160.45, 177.07)), ((2260, 2263),(-40.78, 293.87)), ((2264, 2273),(-112.11, 107.18)),
((2274, 2274),(-12.48, 280.74)), ((2275, 2284),(-127.49, 281.94)), ((2285, 2287),(-85.37, 298.73)),
((2289, 2298),(12.83, 196.15)), ((2306, 2315),(24.38, 318.83)), ((2316, 2316),(-125.88, 244.41)),
((2317, 2317),(-2.76, 137.98)), ((2318, 2327),(-43.4, 280.14)), ((2328, 2329),(83.22, 303.29)),
((2330, 2330),(-56.78, 103.55)), ((2331, 2333),(-31.98, 335.44)), ((2334, 2335),(-116.4, 253.28)),
((2336, 2336),(24.2, 204.24)), ((2337, 2338),(-18.02, 37.22)), ((2339, 2340),(-129.0, 256.56)),
((2341, 2341),(-25.08, 111.38)), ((2342, 2342),(-95.5, 190.77)), ((2343, 2343),(246.94, 272.79)),
((2344, 2348),(-78.21, 186.02)), ((2349, 2349),(-118.13, 257.5)), ((2350, 2359),(-37.93, 220.38)),
((2385, 2394),(110.32, 132.85)), ((2396, 2405),(-68.54, 157.89)), ((2409, 2417),(105.77, 107.74)),
((2419, 2419),(-93.52, -55.16)), ((2420, 2420),(73.72, 192.89)), ((2421, 2430),(-159.01, 25.19)),
((2431, 2431),(-5.13, 43.47)), ((2432, 2433),(-101.62, 102.97)), ((2434, 2443),(70.96, 85.9)),
((2444, 2445),(-40.71, 14.09)), ((2446, 2446),(51.13, 334.34)), ((2447, 2451),(93.48, 124.49)),
((2452, 2455),(88.09, 253.23)), ((2456, 2456),(-112.2, 227.0)), ((2457, 2457),(101.61, 303.03)),
((2458, 2459),(84.87, 185.89)), ((2460, 2461),(-60.81, -46.31)), ((2462, 2463),(-39.62, -1.46)),
((2464, 2465),(92.84, 194.33)), ((2466, 2466),(-17.46, 94.54)), ((2467, 2467),(-138.24, 288.73)),
((2468, 2477),(-120.42, -106.6)), ((2478, 2487),(62.84, 340.1)), ((2488, 2488),(-26.46, 321.91)),
((2489, 2489),(-148.68, 165.87)), ((2490, 2491),(174.62, 295.61)), ((2492, 2492),(-112.31, 197.46)),
((2493, 2502),(225.9, 255.36)), ((2519, 2528),(-15.33, 6.44)), ((2529, 2529),(33.11, 351.19)),
((2530, 2531),(-86.45, 40.55)), ((2532, 2534),(-142.05, 39.45)), ((2535, 2536),(55.61, 64.87)),
((2537, 2538),(1.44, 155.57)), ((2539, 2539),(62.85, 286.41)), ((2540, 2549),(19.58, 138.29)),
((2550, 2551),(-46.1, 189.56)), ((2552, 2552),(-152.99, -14.97)), ((2553, 2555),(-38.82, 323.94)),
((2556, 2560),(-92.74, 105.12)), ((2561, 2561),(48.58, 114.01)), ((2562, 2570),(71.95, 171.12)),
((2571, 2572),(79.0, 298.73)), ((2573, 2575),(43.12, 269.32)), ((2576, 2585),(-70.05, 296.15)),
((2586, 2587),(228.13, 317.77)), ((2588, 2588),(-146.15, -19.22)), ((2589, 2589),(0.13, 339.54)),
((2590, 2591),(151.61, 184.22)), ((2593, 2596),(-59.34, 161.8)), ((2597, 2602),(120.03, 159.0)),
((2604, 2608),(143.12, 144.01)), ((2609, 2609),(-112.3, 335.31)), ((2610, 2610),(-108.37, 27.79)),
((2611, 2620),(-110.09, 223.42)), ((2621, 2621),(-54.23, 37.11)), ((2623, 2623),(-116.41, 28.46)),
((2624, 2626),(332.29, 349.16)), ((2627, 2628),(-90.66, 29.92)), ((2629, 2638),(13.17, 173.22)),
((2639, 2639),(-6.82, 46.09)), ((2640, 2641),(-147.05, 179.4)), ((2643, 2646),(92.28, 271.69)),
((2647, 2647),(266.5, 349.29)), ((2648, 2650),(133.81, 138.02)), ((2651, 2651),(-147.49, 37.5)),
((2652, 2652),(216.14, 301.65)), ((2654, 2660),(69.15, 283.58)), ((2661, 2670),(-83.64, 339.51)),
((2671, 2671),(28.07, 279.53)), ((2672, 2675),(185.3, 328.69)), ((2676, 2685),(328.31, 340.43)),
((2691, 2700),(-46.9, 49.79)), ((2718, 2727),(-84.84, 34.9)), ((2728, 2728),(-128.22, 289.46)),
((2729, 2731),(207.48, 335.14)), ((2732, 2741),(-127.54, -106.28)), ((2790, 2799),(1.74, 118.76)),
((2844, 2853),(245.24, 312.27)), ((2891, 2898),(199.12, 218.22)), ((2899, 2908),(206.15, 243.87)),
((2922, 2925),(99.02, 182.45)), ((2928, 2928),(-31.24, -22.0)), ((2931, 2940),(207.24, 246.39)),
((2945, 2954),(42.26, 337.06)), ((2966, 2975),(54.77, 192.8)), ((2976, 2985),(-66.17, -32.78)),
((2986, 2990),(66.31, 203.22)), ((2991, 3000),(-37.61, 110.98)), ((3005, 3005),(41.91, 351.74)),
((3007, 3016),(94.48, 186.03)), ((3017, 3022),(101.96, 342.95)), ((3029, 3038),(-145.37, -83.52)),
((3045, 3054),(133.3, 147.12)), ((3056, 3056),(6.25, 342.05)), ((3057, 3066),(-18.87, 79.49)),
((3079, 3088),(51.47, 105.3)), ((3089, 3089),(-159.76, 299.47)), ((3090, 3091),(-124.76, -87.54)),
((3092, 3093),(-16.05, -5.91)), ((3094, 3103),(51.18, 315.24)), ((3109, 3117),(-5.08, 122.18)),
((3118, 3118),(41.77, 303.37)), ((3121, 3126),(-79.75, 144.88)), ((3130, 3139),(63.7, 161.99)),
((3140, 3149),(105.46, 232.4)), ((3150, 3150),(80.45, 346.94)), ((3151, 3152),(105.82, 277.34)),
((3153, 3162),(76.29, 301.37)), ((3259, 3268),(291.1, 320.43)), ((3269, 3269),(177.12, 195.17)),
((3270, 3270),(156.21, 237.22)), ((3271, 3273),(-146.98, 338.38)), ((3277, 3278),(243.47, 253.83)),
((3279, 3279),(-86.85, 117.84)), ((3280, 3289),(58.74, 321.11)), ((3290, 3290),(180.06, 210.48)),
((3291, 3292),(232.16, 356.8)), ((3294, 3296),(-146.01, 33.53)), ((3297, 3298),(-90.95, 217.84)),
((3308, 3317),(-0.86, 114.99)), ((3321, 3325),(-29.9, 222.03)), ((3326, 3329),(202.33, 291.65)),
((3379, 3383),(-105.22, -52.79)), ((3397, 3406),(123.49, 241.0)), ((3419, 3428),(-121.63, 183.17)),
((3431, 3440),(-152.99, 164.42)), ((3530, 3539),(115.25, 175.23)), ((3567, 3576),(-84.41, -71.29)),
((3641, 3650),(-134.08, 91.31)), ((3676, 3685),(-3.96, 26.08)), ((3729, 3735),(-128.18, 14.65)),
((3967, 3976),(-71.83, 72.28)), ((4101, 4110),(-64.76, 283.12)), ((4599, 4608),(95.38, 336.93)),
((4854, 4863),(-65.83, 221.94)), ((4984, 4993),(-85.04, 331.89)), ((5048, 5057),(-144.4, -112.44)),
((5139, 5148),(169.9, 296.57)), ((5188, 5197),(-34.95, 122.5)), ((5678, 5687),(99.15, 210.45)),
((5688, 5689),(-150.16, -45.81)), ((5690, 5690),(110.11, 304.13)), ((5693, 5702),(109.75, 165.18)),
((5784, 5786),(165.74, 338.25)), ((5809, 5818),(-144.15, 171.97)), ((5946, 5955),(-78.34, 44.83)),
((5956, 5958),(-107.55, 281.31)), ((5959, 5960),(27.79, 52.54)), ((5961, 5961),(-58.63, 159.34)),
((5962, 5964),(-107.29, -28.71)), ((5965, 5966),(63.84, 252.94)), ((5967, 5967),(-148.06, -63.45)),
((5968, 5969),(-139.13, -29.08)), ((5970, 5970),(-105.44, -20.19)), ((5971, 5973),(-133.97, 321.25)),
((5974, 5977),(-14.97, -0.31)), ((5978, 5979),(244.59, 296.22)), ((5980, 5980),(-94.13, 210.08)),
((5981, 5983),(84.02, 257.35)), ((5984, 5984),(-111.6, 267.07)), ((5985, 5994),(-31.57, -25.32)),
((5995, 5995),(-128.02, 99.57)), ((5996, 5996),(-47.5, -4.08)), ((5997, 5997),(-130.43, -98.35)),
((5998, 5998),(70.46, 324.56)), ((5999, 6001),(217.45, 219.1)), ((6002, 6011),(-94.48, -53.06)),
((6012, 6021),(48.05, 92.8)), ((6022, 6022),(-70.66, -25.79)), ((6023, 6032),(92.5, 252.3)),
((6033, 6034),(-97.76, 105.77)), ((6035, 6044),(141.49, 317.58)), ((6045, 6045),(296.61, 306.7)),
((6046, 6047),(-40.97, 209.71)), ((6048, 6048),(-153.21, -142.99)), ((6049, 6050),(-82.46, -44.12)),
((6051, 6058),(21.48, 246.02)), ((6059, 6061),(-136.71, -59.36)), ((6062, 6062),(116.19, 328.2)),
((6063, 6064),(-141.87, -103.18)), ((6065, 6065),(79.16, 273.79)), ((6066, 6067),(41.23, 243.55)),
((6068, 6068),(147.28, 194.11)), ((6070, 6070),(-71.17, 324.65)), ((6071, 6074),(-2.65, 295.58)),
((6075, 6084),(158.52, 286.53)), ((6094, 6099),(-4.59, 298.87)), ((6100, 6101),(-92.1, -16.9)),
((6102, 6102),(83.32, 297.74)), ((6103, 6112),(-108.6, 83.55)), ((6113, 6113),(16.79, 116.05)),
((6114, 6116),(47.67, 149.11)), ((6117, 6124),(77.7, 136.48)), ((6128, 6135),(-10.51, 110.12)),
((6136, 6136),(-107.29, 334.9)), ((6137, 6139),(-96.54, -74.06)), ((6140, 6146),(-42.05, 13.58)),
((6147, 6156),(13.49, 170.49)), ((6601, 6610),(-137.84, 10.62)), ((6611, 6619),(52.85, 187.86)),
((6620, 6620),(78.59, 134.91)), ((6621, 6630),(-6.41, 100.13)), ((6631, 6631),(-73.48, 3.84)),
((6633, 6633),(-129.55, 112.44)), ((6634, 6639),(-43.02, 311.92)), ((6640, 6640),(-36.81, 281.8)),
((6641, 6641),(217.38, 241.57)), ((6642, 6642),(186.44, 265.59)), ((6643, 6648),(-100.75, 341.95)),
((6649, 6658),(299.11, 350.09)), ((6659, 6659),(235.02, 356.41)), ((6660, 6660),(-125.6, 314.92)),
((6661, 6670),(22.49, 240.64)), ((6671, 6673),(65.9, 214.37)), ((6674, 6678),(-84.02, 255.38)),
((6679, 6682),(-148.24, -36.43)), ((6683, 6684),(-89.0, -79.15)), ((6685, 6694),(-37.75, -22.23)),
((6695, 6697),(-105.39, 117.43)), ((6698, 6702),(2.21, 191.93)), ((6703, 6703),(37.51, 161.38)),
((6705, 6705),(162.72, 262.41)), ((6706, 6707),(-41.2, 193.28)), ((6709, 6713),(120.7, 257.12)),
((6715, 6717),(-159.81, -142.81)), ((6725, 6734),(-45.99, 150.15)), ((6735, 6736),(-101.12, 137.41)),
((6737, 6746),(-135.25, -36.61)), ((6747, 6747),(-73.29, -11.94)), ((6752, 6761),(1.77, 221.34)),
((6762, 6764),(-113.25, 51.91)), ((6774, 6774),(51.06, 115.88)), ((6775, 6783),(245.03, 264.72)),
((6784, 6790),(-46.19, 117.01)), ((6793, 6802),(130.24, 142.92)), ((6803, 6808),(-81.97, 300.77)),
((6809, 6809),(68.32, 302.92)), ((6810, 6811),(38.09, 102.61)), ((6814, 6823),(70.2, 234.85)),
((6824, 6824),(-145.58, -42.34)), ((6825, 6825),(-111.4, 175.37)), ((6826, 6826),(124.97, 351.96)),
((6827, 6827),(165.21, 245.03)), ((6828, 6831),(195.84, 259.1)), ((6832, 6832),(240.15, 240.29)),
((6833, 6833),(-124.91, 287.23)), ((6834, 6834),(80.81, 227.42)), ((6835, 6838),(54.86, 286.76)),
((6839, 6848),(46.99, 222.42)), ((6849, 6849),(-52.87, 228.53)), ((6850, 6850),(69.0, 171.7)),
((6851, 6851),(-41.12, 213.04)), ((6852, 6861),(-124.55, 142.07)), ((6862, 6862),(-16.59, 311.0)),
((6863, 6864),(-114.06, 70.58)), ((6873, 6879),(-102.42, 142.89)), ((6880, 6880),(-89.87, 182.74)),
((6881, 6881),(97.95, 138.29)), ((6882, 6882),(56.99, 200.23)), ((6883, 6884),(112.53, 350.21)),
((6885, 6886),(83.91, 194.46)), ((6887, 6889),(-65.42, 300.53)), ((6890, 6890),(306.68, 333.17)),
((6891, 6897),(-133.95, 183.62)), ((6898, 6903),(-93.98, 303.59)), ((6904, 6913),(121.68, 250.77)),
((6914, 6914),(74.81, 234.35)), ((6915, 6919),(186.8, 293.92)), ((6920, 6920),(55.49, 134.28)),
((6921, 6921),(-5.06, 259.19)), ((6922, 6931),(-23.47, 206.45)), ((6932, 6936),(-123.67, -58.44)),
((6937, 6946),(26.53, 180.49)), ((6952, 6961),(-29.42, 70.54)), ((6976, 6985),(51.71, 202.93)),
((7025, 7034),(182.34, 303.7)), ((7035, 7035),(-27.68, -12.65)), ((7036, 7036),(208.6, 254.86)),
((7037, 7038),(-44.1, 251.69)), ((7039, 7040),(133.42, 297.38)), ((7041, 7050),(-144.38, 238.4)),
((7051, 7053),(22.41, 292.28)), ((7054, 7063),(22.15, 100.62)), ((7064, 7064),(-108.81, 89.56)),
((7065, 7065),(-34.91, 12.72)), ((7066, 7068),(55.35, 96.0)), ((7069, 7069),(70.01, 287.01)),
((7070, 7073),(20.93, 275.74)), ((7074, 7074),(155.56, 166.2)), ((7075, 7075),(1.15, 134.05)),
((7076, 7076),(-102.33, -56.94)), ((7077, 7086),(272.96, 298.35)), ((7087, 7087),(-143.29, 4.73)),
((7088, 7088),(139.36, 268.43)), ((7089, 7098),(177.63, 286.69)), ((7099, 7100),(-116.02, 271.44)),
((7105, 7108),(134.55, 163.14)), ((7109, 7109),(123.88, 181.24)), ((7112, 7116),(326.42, 352.81)),
((7117, 7117),(-70.57, 107.75)), ((7118, 7119),(-146.36, 340.93)), ((7123, 7127),(153.85, 220.13)),
((7128, 7128),(54.22, 162.55)), ((7129, 7129),(-82.34, 43.73)), ((7130, 7133),(-33.68, -27.43)),
((7134, 7139),(9.7, 155.04)), ((7140, 7141),(-120.57, 93.45)), ((7142, 7144),(-27.26, 16.81)),
((7145, 7146),(129.67, 296.37)), ((7147, 7156),(-4.88, 30.86)), ((7157, 7157),(-121.67, -108.0)),
((7158, 7159),(-134.42, 223.07)), ((7160, 7167),(-153.73, 118.52)), ((7168, 7177),(94.92, 280.32)),
((7178, 7183),(157.77, 302.4)), ((7184, 7184),(-112.34, 69.49)), ((7186, 7187),(54.64, 96.15)),
((7188, 7188),(-128.88, 215.28)), ((7189, 7198),(24.93, 110.69)), ((7199, 7199),(36.02, 94.47)),
((7200, 7204),(144.92, 231.12)), ((7205, 7208),(-100.61, 347.0)), ((7209, 7218),(-69.65, 157.62)),
((7219, 7225),(24.59, 157.93)), ((7227, 7229),(52.66, 210.05)), ((7230, 7232),(11.29, 321.19)),
((7233, 7233),(178.46, 320.77)), ((7234, 7243),(19.54, 318.48)), ((7251, 7260),(86.49, 142.53)),
((7261, 7261),(212.11, 280.63)), ((7262, 7262),(-61.95, 130.38)), ((7264, 7269),(-117.46, 346.41)),
((7272, 7281),(-88.68, 317.2)), ((7282, 7283),(74.14, 91.52)), ((7284, 7289),(-150.6, 358.73)),
((7290, 7290),(35.97, 294.39)), ((7291, 7292),(-72.19, 11.58)), ((7293, 7293),(202.41, 226.39)),
((7294, 7297),(39.62, 204.66)), ((7298, 7300),(297.0, 358.46)), ((7301, 7310),(-131.06, 150.52)),
((7311, 7312),(-156.57, -133.84)), ((7313, 7315),(-113.4, 43.29)), ((7316, 7316),(73.98, 331.08)),
((7317, 7319),(-146.79, 277.7)), ((7320, 7325),(54.57, 122.95)), ((7326, 7326),(3.3, 43.69)),
((7327, 7328),(-29.49, 268.08)), ((7329, 7329),(-77.18, 261.62)), ((7330, 7339),(-157.81, 115.79)),
((7340, 7340),(-70.34, 153.68)), ((7341, 7350),(97.02, 317.16)), ((7351, 7351),(-23.52, -10.34)),
((7352, 7355),(-98.75, 339.51)), ((7356, 7356),(-37.95, 91.63)), ((7357, 7357),(-49.42, 252.27)),
((7358, 7367),(-130.41, 96.0)), ((7368, 7372),(-89.5, 320.22)), ((7373, 7377),(-119.17, 18.04)),
((7378, 7378),(-38.89, 285.95)), ((7379, 7379),(-116.21, 17.45)), ((7380, 7389),(233.41, 346.44)),
((7390, 7390),(-111.49, 281.87)), ((7391, 7392),(-113.79, 314.81)), ((7393, 7393),(-13.13, 228.41)),
((7394, 7402),(-94.67, -20.88)), ((7403, 7412),(-92.18, 35.02)), ((7413, 7422),(145.44, 357.13)),
((7423, 7427),(-18.89, 351.51)), ((7428, 7429),(15.35, 34.37)), ((7430, 7439),(-99.48, 157.84)),
((7440, 7444),(27.25, 175.77)), ((7445, 7448),(-114.97, -80.87)), ((7459, 7468),(-105.32, 188.72)),
((7473, 7477),(-130.23, 69.23)), ((7488, 7497),(12.05, 120.86)), ((7499, 7500),(194.7, 283.0)),
((7505, 7511),(-20.95, 180.79)), ((7515, 7524),(11.7, 110.3)), ((7536, 7541),(54.91, 225.24)),
((7542, 7544),(34.73, 259.88)), ((7545, 7554),(-134.92, 108.72)), ((7555, 7557),(36.77, 78.18)),
((7558, 7558),(92.23, 171.4)), ((7560, 7562),(81.42, 234.64)), ((7563, 7569),(271.26, 285.11)),
((7570, 7570),(-30.2, 15.49)), ((7571, 7571),(-42.75, 287.14)), ((7572, 7581),(-120.45, 70.44)),
((7582, 7582),(32.17, 256.7)), ((7583, 7583),(134.32, 187.21)), ((7584, 7585),(225.97, 231.97)),
((7586, 7595),(-121.92, -8.46)), ((7596, 7605),(227.04, 286.3)), ((7618, 7619),(-113.23, -75.45)),
((7620, 7629),(193.84, 239.64)), ((7630, 7632),(-43.13, 94.27)), ((7633, 7642),(-85.34, 19.75)),
((7643, 7652),(-13.24, 148.21)), ((7653, 7654),(0.04, 175.51)), ((7655, 7655),(132.43, 292.94)),
((7656, 7665),(-34.21, 174.3)), ((7666, 7666),(136.81, 239.16)), ((7667, 7667),(-38.19, 49.32)),
((7668, 7668),(6.83, 136.18)), ((7669, 7669),(-17.92, 90.87)), ((7670, 7672),(5.75, 192.22)),
((7673, 7673),(-77.42, 24.35)), ((7674, 7677),(-157.79, 110.41)), ((7678, 7686),(-149.29, 215.22)),
((7687, 7688),(-136.24, 252.76)), ((7689, 7689),(-100.17, 276.38)), ((7690, 7699),(222.07, 358.39)),
((7700, 7709),(-159.57, 82.07)), ((7710, 7710),(-61.17, 84.14)), ((7711, 7711),(114.19, 285.73)),
((7712, 7714),(48.05, 166.8)), ((7715, 7716),(150.99, 342.59)), ((7717, 7717),(-142.77, 218.84)),
((7718, 7727),(-51.15, 323.41)), ((7728, 7737),(-90.77, 251.07)), ((7738, 7747),(137.88, 270.84)),
((7749, 7750),(-19.6, 71.85)), ((7751, 7760),(240.84, 344.38)), ((7770, 7775),(-0.73, 90.96)),
((7776, 7785),(49.19, 352.22)), ((7806, 7807),(-82.51, 166.23)), ((7828, 7835),(113.44, 119.6)),
((7837, 7838),(30.85, 326.29)), ((7841, 7846),(-63.49, 171.74)), ((7848, 7850),(-86.11, 60.12)),
((7857, 7862),(7.72, 198.52)), ((7863, 7864),(276.24, 349.38)), ((7865, 7865),(239.97, 263.97)),
((7866, 7872),(104.09, 221.94)), ((7874, 7883),(340.72, 357.81)), ((7884, 7884),(-58.43, 26.35)),
((7885, 7886),(-124.11, -74.34)), ((7887, 7887),(-148.16, 106.21)), ((7888, 7888),(125.59, 127.05)),
((7889, 7889),(26.9, 200.98)), ((7890, 7890),(-61.33, 152.52)), ((7891, 7893),(101.06, 258.26)),
((7894, 7896),(-67.8, 305.14)), ((7900, 7909),(12.37, 162.78)), ((7910, 7912),(39.89, 350.38)),
((7913, 7916),(45.81, 178.61)), ((7917, 7926),(29.59, 244.66)), ((7927, 7930),(-16.2, 19.98)),
((7931, 7934),(260.86, 282.11)), ((7936, 7937),(-43.65, 141.06)), ((7938, 7938),(88.93, 302.24)),
((7939, 7948),(-120.04, 263.53)), ((7949, 7954),(21.36, 121.83)), ((7955, 7956),(-133.95, -53.95)),
((7957, 7966),(-89.53, 148.24)), ((7967, 7967),(-135.84, -97.06)), ((7968, 7970),(247.66, 256.69)),
((7974, 7978),(-141.22, 126.84)), ((7979, 7981),(73.49, 336.63)), ((7982, 7991),(-152.42, -99.61)),
((7992, 7992),(-10.09, 22.69)), ((7993, 7994),(-27.48, 319.88)), ((7995, 7996),(52.27, 266.26)),
((7997, 7997),(-121.34, 197.12)), ((7998, 8000),(6.1, 347.3)), ((8001, 8001),(-118.83, 2.62)),
((8002, 8003),(223.96, 300.93)), ((8008, 8017),(48.71, 284.02)), ((8019, 8019),(163.24, 229.77)),
((8023, 8029),(-77.56, 159.72)), ((8030, 8039),(-102.98, 144.05)), ((8040, 8043),(163.63, 221.52)),
((8066, 8075),(129.73, 200.54)), ((8077, 8086),(67.07, 256.6)), ((8113, 8120),(92.69, 207.42)),
((8121, 8126),(-140.73, 331.85)), ((8127, 8129),(-25.3, 359.48)), ((8130, 8131),(83.29, 327.51)),
((8132, 8139),(22.09, 187.79)), ((8140, 8149),(-142.46, -72.83)), ((8150, 8150),(-37.3, 47.77)),
((8153, 8161),(-142.42, -61.34)), ((8162, 8163),(72.03, 105.35)), ((8164, 8165),(159.79, 225.87)),
((8166, 8168),(-119.27, -14.07)), ((8169, 8169),(-158.71, 326.86)), ((8170, 8170),(-70.95, 66.37)),
((8171, 8180),(-38.7, 48.1)), ((8181, 8181),(-153.2, -148.94)), ((8182, 8187),(280.88, 304.76)),
((8188, 8188),(-58.42, 151.49)), ((8189, 8191),(-144.23, 315.3)), ((8192, 8192),(-105.2, 226.18)),
((8193, 8194),(89.84, 205.68)), ((8195, 8204),(-156.05, 102.57)), ((8206, 8206),(-98.35, 95.9)),
((8207, 8216),(-146.87, -78.34)), ((8217, 8218),(-122.57, 238.94)), ((8219, 8228),(181.92, 350.39)),
((8229, 8230),(198.93, 279.72)), ((8231, 8240),(-80.03, 90.76)), ((8241, 8243),(-19.2, 137.69)),
((8244, 8244),(-112.63, 166.01)), ((8245, 8245),(267.04, 269.8)), ((8246, 8246),(328.82, 329.61)),
((8247, 8248),(79.05, 358.78)), ((8249, 8250),(-57.61, 99.18)), ((8252, 8252),(17.92, 261.23)),
((8253, 8255),(37.65, 54.11)), ((8256, 8259),(-128.72, -84.63)), ((8260, 8263),(323.57, 360.19)),
((8269, 8278),(-78.98, 318.25)), ((8280, 8280),(-104.39, 218.71)), ((8281, 8290),(256.47, 289.48)),
((8296, 8297),(-65.5, 299.99)), ((8304, 8313),(45.32, 230.91)), ((8314, 8314),(186.84, 292.25)),
((8315, 8324),(115.58, 133.43)), ((8327, 8331),(5.87, 296.26)), ((8338, 8347),(88.35, 212.81)),
((8382, 8391),(76.58, 143.85)), ((8392, 8393),(-134.39, 321.47)), ((8395, 8396),(-111.42, 182.77)),
((8397, 8398),(51.6, 244.59)), ((8399, 8401),(19.79, 169.06)), ((8405, 8408),(-29.51, 330.82)),
((8409, 8409),(296.54, 308.85)), ((8410, 8410),(-8.11, 226.29)), ((8411, 8412),(-55.13, -30.76)),
((8413, 8422),(0.5, 47.42)), ((8423, 8432),(44.1, 85.71)), ((8434, 8434),(-33.29, 289.18)),
((8436, 8436),(-52.94, 201.4)), ((8437, 8439),(82.61, 213.37)), ((8440, 8440),(-59.31, 263.39)),
((8441, 8441),(-18.22, 119.57)), ((8442, 8447),(45.87, 218.98)), ((8448, 8457),(69.85, 335.77)),
((8458, 8458),(131.29, 269.74)), ((8459, 8459),(112.99, 256.92)), ((8460, 8469),(-71.84, 183.02)),
((8470, 8470),(21.1, 100.81)), ((8471, 8473),(66.64, 112.72)), ((8474, 8478),(-20.92, 311.39)),
((8480, 8482),(-143.51, 61.1)), ((8483, 8492),(217.58, 305.17)), ((8493, 8502),(249.2, 283.24)),
((8503, 8503),(-156.69, -133.18)), ((8504, 8507),(6.15, 225.99)), ((8512, 8521),(286.46, 338.52)),
((8525, 8532),(-97.74, -34.65)), ((8533, 8533),(-61.15, 191.09)), ((8534, 8543),(236.38, 318.71)),
((8544, 8545),(-103.17, 109.74)), ((8548, 8557),(126.19, 193.84)), ((8652, 8661),(103.1, 165.68)),
((8663, 8664),(141.18, 355.23)), ((8668, 8669),(-49.64, 350.79)), ((8670, 8670),(-113.16, 250.88)),
((8671, 8672),(66.13, 152.85)), ((8673, 8677),(-42.03, 331.46)), ((8683, 8692),(109.1, 251.45)),
((8712, 8719),(-61.07, 179.4)), ((8726, 8735),(-98.38, -38.43)), ((8738, 8747),(-109.46, 204.09)),
((8778, 8787),(-16.05, 345.25)), ((8802, 8811),(-93.46, 90.97)), ((8812, 8812),(-2.43, 218.34)),
((8813, 8813),(-150.48, -141.79)), ((8814, 8815),(-94.15, 130.9)), ((8816, 8816),(-15.63, 233.36)),
((8817, 8817),(-94.97, -5.23)), ((8818, 8827),(153.65, 161.62)), ((8835, 8838),(236.98, 253.47)),
((8839, 8841),(-68.55, -14.42)), ((8842, 8842),(-27.78, 59.15)), ((8843, 8852),(-98.71, 66.26)),
((8853, 8853),(54.68, 179.51)), ((8854, 8854),(64.93, 95.39)), ((8855, 8856),(1.06, 70.42)),
((8858, 8858),(-12.13, 27.31)), ((8859, 8859),(-105.98, 228.61)), ((8860, 8861),(55.36, 70.18)),
((8862, 8862),(-130.97, 248.22)), ((8863, 8863),(71.05, 348.5)), ((8864, 8870),(-18.68, 92.27)),
((8871, 8874),(119.75, 298.37)), ((8875, 8875),(116.14, 214.62)), ((8876, 8881),(14.69, 307.03)),
((8882, 8888),(195.83, 288.91)), ((8889, 8890),(-141.43, 266.55)), ((8891, 8892),(71.16, 315.22)),
((8894, 8896),(-117.25, 19.98)), ((8897, 8898),(15.15, 51.67)), ((8899, 8908),(-135.07, 45.55)),
((8909, 8909),(65.1, 348.5)), ((8910, 8911),(167.51, 288.91)), ((8912, 8912),(-53.61, 58.05)),
((8913, 8922),(151.5, 240.66)), ((8923, 8923),(-54.99, 165.15)), ((8925, 8927),(101.56, 165.4)),
((8928, 8928),(-131.06, 267.61)), ((8929, 8929),(13.25, 359.28)), ((8930, 8939),(-108.66, 194.35)),
((8940, 8942),(-155.67, 293.24)), ((8943, 8944),(234.76, 280.21)), ((8945, 8948),(214.57, 242.59)),
((8949, 8958),(237.03, 268.27)), ((8959, 8959),(-1.63, 24.42)), ((8960, 8969),(40.83, 244.91)),
((8971, 8973),(-71.09, 34.37)), ((8974, 8975),(-87.86, 23.03)), ((8977, 8979),(-125.93, 134.65)),
((8981, 8983),(-38.94, 126.97)), ((8984, 8985),(-20.64, 134.86)), ((8986, 8987),(-135.35, -18.54)),
((8988, 8989),(-135.2, -54.98)), ((8990, 8999),(148.49, 311.5)), ((9000, 9002),(115.37, 120.34)),
((9003, 9003),(-71.15, 316.37)), ((9004, 9013),(51.16, 213.32)), ((9014, 9016),(-23.34, 99.37)),
((9017, 9026),(-87.86, 160.14)), ((9027, 9028),(107.93, 221.32)), ((9029, 9038),(35.0, 293.37)),
((9039, 9040),(113.5, 123.64)), ((9041, 9043),(-120.64, 56.86)), ((9044, 9044),(-2.07, 179.78)),
((9045, 9046),(99.33, 171.68)), ((9047, 9048),(-138.89, -58.2)), ((9049, 9049),(-155.31, 325.6)),
((9050, 9050),(-68.44, 249.4)), ((9051, 9060),(-144.07, 318.76)), ((9131, 9138),(219.3, 285.33)),
((9139, 9148),(-59.2, 357.5)), ((9149, 9149),(8.06, 306.44)), ((9150, 9151),(86.21, 296.03)),
((9152, 9154),(-16.51, 56.62)), ((9155, 9155),(49.66, 149.1)), ((9156, 9156),(-92.97, 81.64)),
((9157, 9162),(44.85, 114.81)), ((9163, 9166),(-59.9, 336.36)), ((9167, 9168),(-50.41, 107.83)),
((9169, 9169),(-67.98, 153.11)), ((9170, 9179),(105.44, 156.97)), ((9180, 9181),(14.7, 157.51)),
((9182, 9191),(120.86, 254.55)), ((9213, 9222),(24.9, 326.42)), ((9223, 9224),(202.74, 353.42)),
((9225, 9232),(-153.97, 316.35)), ((9233, 9242),(123.68, 141.61)), ((9265, 9274),(-148.44, 338.4)),
((9275, 9276),(-97.3, -36.62)), ((9277, 9280),(-140.42, 209.24)), ((9282, 9282),(-69.38, 133.35)),
((9287, 9296),(189.85, 315.13)), ((9297, 9297),(27.66, 54.67)), ((9298, 9298),(243.82, 289.59)),
((9299, 9308),(32.08, 85.27)), ((9309, 9309),(-111.34, -10.99)), ((9310, 9319),(-151.91, 110.33)),
((9420, 9429),(146.5, 237.9)), ((9430, 9430),(4.02, 75.47)), ((9431, 9431),(-27.03, 61.51)),
((9432, 9437),(93.6, 356.82)), ((9438, 9447),(53.31, 295.5)), ((9459, 9468),(98.78, 110.53)),
((9531, 9540),(123.08, 242.45)), ((9561, 9570),(166.9, 333.09)), ((9756, 9765),(100.83, 172.91)),
((9839, 9848),(20.69, 196.12))))
	)

        val n: Real = x.length()
        val y = x.fold(0.0)((acc: Real, i: Real) => acc + i)
        val avg = y / n

        val z = x.fold(0.0)((acc: Real, i: Real) => {
            acc + pow((i - avg), 2)
        })
        sqrt(z / n)
    }


}