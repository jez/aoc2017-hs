module AoC
  ( day1Input
  , day1Pt1
  , day1Pt2
  , sumEqualAdjacentPairs
  , sumEqualOppositePairs

  , day2Input
  , day2Pt1
  , day2Pt2
  , sumOfDifferences
  , sumOfDivisibles

  ) where

import AoC.InverseCaptcha (sumEqualAdjacentPairs, sumEqualOppositePairs)
import AoC.CorruptionChecksum (sumOfDifferences, sumOfDivisibles)

day1Pt1 = sumEqualAdjacentPairs
day1Pt2 = sumEqualOppositePairs

day1Input :: String
day1Input = "5672987533353956199629683941564528646262567117433461547747793928322958646779832484689174151918261551689221756165598898428736782194511627829355718493723961323272136452517987471351381881946883528248611611258656199812998632682668749683588515362946994415852337196718476219162124978836537348924591957188827929753417884942133844664636969742547717228255739959316351852731598292529837885992781815131876183578461135791315287135243541659853734343376618419952776165544829717676988897684141328138348382882699672957866146524759879236555935723655326743713542931693477824289283542468639522271643257212833248165391957686226311246517978319253977276663825479144321155712866946255992634876158822855382331452649953283788863248192338245943966269197421474555779135168637263279579842885347152287275679811576594376535226167894981226866222987522415785244875882556414956724976341627123557214837873872723618395529735349273241686548287549763993653379539445435319698825465289817663294436458194867278623978745981799283789237555242728291337538498616929817268211698649236646127899982839523784837752863458819965485149812959121884771849954723259365778151788719941888128618552455879369919511319735525621198185634342538848462461833332917986297445388515717463168515123732455576143447454835849565757773325367469763383757677938748319968971312267871619951657267913817242485559771582167295794259441256284168356292785568858527184122231262465193612127961685513913835274823892596923786613299747347259254823531262185328274367529265868856512185135329652635938373266759964119863494798222245536758792389789818646655287856173534479551364115976811459677123592747375296313667253413698823655218254168196162883437389718167743871216373164865426458794239496224858971694877159591215772938396827435289734165853975267521291574436567193473814247981877735223376964125359992555885137816647382139596646856417424617847981855532914872251686719394341764324395254556782277426326331441981737557262581762412544849689472281645835957667217384334435391572985228286537574388834835693416821419655967456137395465649249256572866516984318344482684936625486311718525523265165"

day2Pt1 :: [[Int]] -> Int
day2Pt1 = sumOfDifferences

day2Pt2 :: [[Int]] -> Int
day2Pt2 = sumOfDivisibles

day2Input :: [[Int]]
day2Input =
  [ [3093 , 749  , 3469 , 142  , 2049 , 3537 , 1596 , 3035 , 2424 , 3982 , 3290 , 125  , 249  , 131  , 118  , 3138]
  , [141  , 677  , 2705 , 2404 , 2887 , 2860 , 1123 , 2714 , 117  , 1157 , 2607 , 1800 , 153  , 130  , 1794 , 3272]
  , [182  , 93   , 2180 , 114  , 103  , 1017 , 95   , 580  , 2179 , 2470 , 2487 , 2806 , 1574 , 1325 , 1898 , 1706]
  , [3753 , 233  , 3961 , 3747 , 3479 , 3597 , 1303 , 2612 , 4043 , 1815 , 3318 , 737  , 197  , 3943 , 239  , 254]
  , [113  , 147  , 961  , 157  , 3514 , 3045 , 1270 , 3528 , 1369 , 3377 , 492  , 156  , 1410 , 3251 , 1839 , 1249]
  , [3948 , 3651 , 888  , 3631 , 253  , 220  , 4266 , 1284 , 3595 , 237  , 2138 , 3799 , 2319 , 254  , 267  , 1182]
  , [399  , 446  , 795  , 653  , 154  , 762  , 140  , 487  , 750  , 457  , 730  , 150  , 175  , 841  , 323  , 492]
  , [999  , 979  , 103  , 99   , 1544 , 1404 , 100  , 1615 , 840  , 92   , 1552 , 1665 , 1686 , 76   , 113  , 1700]
  , [4049 , 182  , 3583 , 1712 , 200  , 3326 , 3944 , 715  , 213  , 1855 , 2990 , 3621 , 2560 , 842  , 249  , 2082]
  , [2610 , 4749 , 2723 , 2915 , 2189 , 3911 , 124  , 164  , 1895 , 3095 , 3992 , 134  , 127  , 4229 , 3453 , 4428]
  , [105  , 692  , 101  , 150  , 193  , 755  , 84   , 185  , 622  , 851  , 706  , 251  , 86   , 408  , 774  , 831]
  , [238  , 217  , 224  , 1409 , 1850 , 2604 , 363  , 265  , 596  , 2933 , 2641 , 2277 , 803  , 2557 , 1399 , 237]
  , [304  , 247  , 192  , 4369 , 997  , 5750 , 85   , 1248 , 4718 , 3888 , 5228 , 5116 , 5880 , 5348 , 6052 , 245]
  , [238  , 373  , 228  , 395  , 86   , 59   , 289  , 87   , 437  , 384  , 233  , 79   , 470  , 403  , 441  , 352]
  , [151  , 3473 , 1435 , 87   , 1517 , 1480 , 140  , 2353 , 1293 , 118  , 163  , 3321 , 2537 , 3061 , 1532 , 3402]
  , [127  , 375  , 330  , 257  , 220  , 295  , 145  , 335  , 304  , 165  , 151  , 141  , 289  , 256  , 195  , 272]
  ]

