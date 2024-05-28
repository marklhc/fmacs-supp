Example 1: $f_ ext{MACS}$ Effect Size with Alignment Optimization
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Load Packages

``` r
library(readr)
library(here)
```

    ## here() starts at /media/hokchiol/DATADRIVE2/Insync/hokchiol@usc.edu/OneDrive Biz/Research/fmacs

``` r
library(sirt)
```

    ## - sirt 4.2-40 (2024-03-19 13:56:57)

``` r
library(lavaan)
```

    ## This is lavaan 0.6-17
    ## lavaan is FREE software! Please report any bugs.

``` r
library(pinsearch)
```

## Import Data

``` r
# Analyses from Asparouhov & Muthen (2014); data from https://www.statmodel.com/Alignment.shtml
traco_dat <- readr::read_delim(
    here::here("analysis", "webnote18data", "ess05Traco.dat"),
    col_names = c(
        "country", "essround", "ipfrule", "ipmodst",
        "ipbhprp", "imptrad"
    )
)
```

    ## Rows: 50781 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## dbl (6): country, essround, ipfrule, ipmodst, ipbhprp, imptrad
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Recode missing data
traco_dat[3:6] <- lapply(traco_dat[3:6],
    FUN = \(x, list = 7:9) { x[x %in% list] <- NA; x }
)
```

## Descriptive Statistics

``` r
library(modelsummary)
```

    ## Version 2.0.0 of `modelsummary`, to be released soon, will introduce a
    ##   breaking change: The default table-drawing package will be `tinytable`
    ##   instead of `kableExtra`. All currently supported table-drawing packages
    ##   will continue to be supported for the foreseeable future, including
    ##   `kableExtra`, `gt`, `huxtable`, `flextable, and `DT`.
    ##   
    ##   You can always call the `config_modelsummary()` function to change the
    ##   default table-drawing package in persistent fashion. To try `tinytable`
    ##   now:
    ##   
    ##   config_modelsummary(factory_default = 'tinytable')
    ##   
    ##   To set the default back to `kableExtra`:
    ##   
    ##   config_modelsummary(factory_default = 'kableExtra')

``` r
traco_dat |>
    datasummary(
        formula = Factor(country) ~ (ipfrule + ipmodst + ipbhprp + imptrad) *
            (N + Mean + SD)
    )
```

| country | ipfrule / N | ipfrule / Mean | ipfrule / SD | ipmodst / N | ipmodst / Mean | ipmodst / SD | ipbhprp / N | ipbhprp / Mean | ipbhprp / SD | imptrad / N | imptrad / Mean | imptrad / SD |
|:--------|------------:|---------------:|-------------:|------------:|---------------:|-------------:|------------:|---------------:|-------------:|------------:|---------------:|-------------:|
| 2       |        1697 |           3.26 |         1.27 |        1702 |           2.51 |         1.09 |        1699 |           2.59 |         1.11 |        1702 |           2.68 |         1.24 |
| 3       |        2363 |           2.53 |         1.23 |        2405 |           2.57 |         1.26 |        2383 |           2.26 |         1.09 |        2404 |           2.14 |         1.11 |
| 4       |        1487 |           3.42 |         1.40 |        1491 |           2.50 |         1.16 |        1488 |           2.76 |         1.29 |        1493 |           2.73 |         1.38 |
| 5       |        1044 |           2.85 |         1.36 |        1063 |           2.25 |         1.11 |        1057 |           2.11 |         1.06 |        1062 |           1.76 |         0.93 |
| 6       |        2320 |           2.80 |         1.13 |        2329 |           2.99 |         1.20 |        2335 |           2.62 |         1.11 |        2332 |           2.81 |         1.30 |
| 7       |        2994 |           3.31 |         1.40 |        3010 |           2.64 |         1.25 |        3003 |           2.78 |         1.27 |        3011 |           2.81 |         1.41 |
| 8       |        1555 |           3.00 |         1.29 |        1565 |           3.46 |         1.35 |        1561 |           2.66 |         1.28 |        1565 |           2.71 |         1.40 |
| 9       |        1793 |           3.27 |         1.25 |        1793 |           2.81 |         1.21 |        1793 |           2.59 |         1.11 |        1793 |           2.86 |         1.34 |
| 10      |        1864 |           3.04 |         1.31 |        1881 |           2.24 |         1.04 |        1880 |           2.31 |         1.04 |        1884 |           2.72 |         1.38 |
| 11      |        1648 |           2.98 |         1.29 |        1646 |           3.09 |         1.22 |        1644 |           2.77 |         1.16 |        1647 |           3.02 |         1.38 |
| 12      |        1711 |           3.97 |         1.42 |        1722 |           2.39 |         1.22 |        1720 |           2.74 |         1.39 |        1722 |           3.28 |         1.60 |
| 13      |        2371 |           3.29 |         1.37 |        2377 |           2.74 |         1.25 |        2381 |           2.57 |         1.21 |        2380 |           2.82 |         1.40 |
| 14      |        2654 |           3.26 |         1.43 |        2672 |           2.43 |         1.16 |        2664 |           2.25 |         1.12 |        2668 |           2.10 |         1.12 |
| 15      |        1606 |           3.22 |         1.40 |        1613 |           2.85 |         1.31 |        1600 |           2.40 |         1.14 |        1597 |           2.28 |         1.22 |
| 16      |        1470 |           3.47 |         1.40 |        1473 |           2.57 |         1.26 |        1470 |           2.34 |         1.16 |        1474 |           2.43 |         1.31 |
| 17      |        2404 |           3.01 |         1.38 |        2406 |           2.67 |         1.30 |        2384 |           2.60 |         1.28 |        2400 |           2.71 |         1.39 |
| 18      |        2223 |           2.50 |         1.29 |        2217 |           2.37 |         1.17 |        2202 |           2.33 |         1.17 |        2212 |           2.62 |         1.44 |
| 21      |        1800 |           2.91 |         1.16 |        1799 |           3.24 |         1.24 |        1777 |           2.81 |         1.10 |        1785 |           2.77 |         1.25 |
| 22      |        1529 |           2.98 |         1.18 |        1530 |           3.47 |         1.30 |        1532 |           2.50 |         1.14 |        1532 |           2.88 |         1.31 |
| 23      |        1717 |           2.48 |         1.07 |        1725 |           2.80 |         1.21 |        1724 |           2.38 |         1.02 |        1732 |           2.21 |         1.07 |
| 24      |        2133 |           3.18 |         1.10 |        2144 |           2.62 |         1.10 |        2142 |           2.99 |         1.03 |        2142 |           2.81 |         1.05 |
| 25      |        2522 |           3.05 |         1.30 |        2568 |           2.63 |         1.23 |        2561 |           2.50 |         1.14 |        2560 |           2.42 |         1.19 |
| 26      |        1457 |           3.59 |         1.34 |        1457 |           2.91 |         1.23 |        1461 |           3.24 |         1.34 |        1466 |           3.19 |         1.42 |
| 27      |        1366 |           2.99 |         1.29 |        1371 |           2.11 |         0.91 |        1374 |           2.23 |         1.04 |        1374 |           2.41 |         1.13 |
| 28      |        1801 |           2.53 |         1.06 |        1816 |           2.50 |         1.08 |        1805 |           2.35 |         1.09 |        1813 |           2.36 |         1.18 |
| 30      |        1787 |           2.92 |         1.38 |        1780 |           3.33 |         1.47 |        1792 |           2.68 |         1.30 |        1823 |           2.47 |         1.30 |

Cross-Tabulation

``` r
with(traco_dat, table(country, ipfrule, useNA = "ifany"))
```

    ##        ipfrule
    ## country   1   2   3   4   5   6 <NA>
    ##      2   95 454 472 329 283  64    7
    ##      3  487 838 573 275 136  54   71
    ##      4  124 331 333 272 352  75   19
    ##      5  149 358 240 139 115  43   39
    ##      6  242 795 699 396 156  32   66
    ##      7  229 826 670 500 597 172   37
    ##      8  166 487 366 285 224  27   21
    ##      9   86 494 504 324 337  48    0
    ##      10 180 579 478 304 263  60   21
    ##      11 186 466 479 277 184  56  230
    ##      12 105 213 265 373 547 208   17
    ##      13 188 646 502 463 478  94   51
    ##      14 257 693 628 447 445 184   61
    ##      15 177 394 371 308 279  77   43
    ##      16 121 281 362 319 270 117   91
    ##      17 352 635 547 451 334  85  172
    ##      18 550 721 487 264 152  49   71
    ##      21 137 603 586 286 140  48   29
    ##      22 109 533 367 332 171  17   19
    ##      23 265 753 421 172 100   6   34
    ##      24 127 428 810 506 232  30   17
    ##      25 244 730 723 387 349  89   73
    ##      26  69 293 337 318 349  91   40
    ##      27 104 487 386 150 189  50   37
    ##      28 271 680 581 175  77  17   55
    ##      30 325 417 438 341 209  57  144

``` r
with(traco_dat, table(country, ipmodst, useNA = "ifany"))
```

    ##        ipmodst
    ## country    1    2    3    4    5    6 <NA>
    ##      2   254  731  431  181   91   14    2
    ##      3   523  765  608  279  193   37   29
    ##      4   264  623  339  138  117   10   15
    ##      5   268  470  182   86   48    9   20
    ##      6   242  597  740  477  230   43   57
    ##      7   467 1222  643  354  260   64   21
    ##      8   111  338  348  328  375   65   11
    ##      9   172  706  463  235  180   37    0
    ##      10  470  798  383  162   63    5    4
    ##      11  142  419  515  331  205   34  232
    ##      12  485  539  346  254   82   16    6
    ##      13  321  933  483  364  235   41   45
    ##      14  589 1018  606  289  138   32   43
    ##      15  245  484  400  278  156   50   36
    ##      16  311  492  365  166  108   31   88
    ##      17  486  756  531  390  187   56  170
    ##      18  619  663  567  258   92   18   77
    ##      21  118  413  574  382  242   70   30
    ##      22   87  336  355  320  391   41   18
    ##      23  185  645  440  267  154   34   26
    ##      24  395  575  712  385   68    9    6
    ##      25  498  794  716  329  191   40   27
    ##      26  137  513  380  231  167   29   40
    ##      27  309  746  217   59   36    4   32
    ##      28  332  637  549  218   65   15   40
    ##      30  251  311  368  402  343  105  151

``` r
with(traco_dat, table(country, ipbhprp, useNA = "ifany"))
```

    ##        ipbhprp
    ## country    1    2    3    4    5    6 <NA>
    ##      2   208  743  429  202   91   26    5
    ##      3   613  952  522  194   75   27   51
    ##      4   224  525  349  188  173   29   18
    ##      5   299  515  136   56   39   12   26
    ##      6   341  828  704  320  113   29   51
    ##      7   375 1160  685  377  341   65   28
    ##      8   289  547  331  224  145   25   15
    ##      9   202  810  471  159  129   22    0
    ##      10  394  846  404  150   80    6    5
    ##      11  197  554  492  261  114   26  234
    ##      12  391  449  353  316  159   52    8
    ##      13  385 1025  431  335  167   38   41
    ##      14  703 1114  495  216  109   27   51
    ##      15  348  635  349  183   67   18   49
    ##      16  383  523  338  150   54   22   91
    ##      17  518  765  507  369  187   38  192
    ##      18  596  773  490  224   90   29   92
    ##      21  149  633  570  282  114   29   52
    ##      22  245  694  291  196   94   12   16
    ##      23  284  812  399  150   70    9   27
    ##      24  143  530  851  473  119   26    8
    ##      25  469  985  679  264  121   43   34
    ##      26  118  398  329  302  255   59   36
    ##      27  280  725  224   72   59   14   29
    ##      28  373  764  450  127   62   29   51
    ##      30  387  474  471  281  140   39  139

``` r
with(traco_dat, table(country, imptrad, useNA = "ifany"))
```

    ##        imptrad
    ## country    1    2    3    4    5    6 <NA>
    ##      2   248  664  409  203  130   48    2
    ##      3   807  828  514  165   57   33   30
    ##      4   300  471  327  179  166   50   13
    ##      5   501  392  112   36   17    4   21
    ##      6   385  663  646  351  218   69   54
    ##      7   502 1039  645  337  344  144   20
    ##      8   339  467  338  198  170   53   11
    ##      9   241  621  442  199  225   65    0
    ##      10  388  595  387  252  200   62    1
    ##      11  214  460  427  262  199   85  231
    ##      12  293  358  271  352  278  170    6
    ##      13  399  830  443  292  346   70   42
    ##      14  979  893  474  222   80   20   47
    ##      15  488  559  303  143   76   28   52
    ##      16  428  439  318  168   78   43   87
    ##      17  568  639  471  418  245   59  176
    ##      18  588  609  444  289  181  101   82
    ##      21  252  600  461  297  124   51   44
    ##      22  200  529  317  262  189   35   16
    ##      23  447  775  313  122   54   21   19
    ##      24  215  622  809  374  100   22    8
    ##      25  602  918  627  239  131   43   35
    ##      26  150  402  347  256  214   97   31
    ##      27  238  663  278   93   75   27   29
    ##      28  450  679  415  150   89   30   43
    ##      30  509  536  364  271  109   34  108

## Alignment

``` r
config_res <- sirt::invariance_alignment_cfa_config(
    traco_dat[c("ipmodst", "imptrad", "ipfrule", "ipbhprp")],
    group = traco_dat$country, missing = "ML"
)
```

    ## Compute CFA for group 1 | model 2PM

    ## Loading required namespace: MASS

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   71 1451

    ## Compute CFA for group 2 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   236 729 973 1334 1344 1697 1736 1948 2070

    ## Compute CFA for group 3 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   495 646 849 1030 1443 1474 1498

    ## Compute CFA for group 4 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   625 984 985 986 987 989 990 992 993 994 995 996 997 998 999 1000 1001

    ## Compute CFA for group 5 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   50 52 214 287 305 317 365 449 528 739 746 758 769 876 883 994 1001 1006 1008 1033 1092 1174 1185 1238 1244 1255 1298 1314 1340 1359 1489 1541 1621 1661 1779 1798 1872 1948 1970 2015 2154 2200 2202

    ## Compute CFA for group 6 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   1388 1428 1832 2126 2326 2345 2810 2869 2996 3007

    ## Compute CFA for group 7 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   72 382 472 534 804 812 854 1076 1149 1385

    ## Compute CFA for group 8 | model 2PM
    ## Compute CFA for group 9 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   395

    ## Compute CFA for group 10 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   35 45 49 60 68 69 87 93 107 117 122 129 143 144 182 252 292 325 326 354 363 380 399 400 401 422 428 432 433 436 437 448 464 465 475 482 487 493 502 504 514 518 536 553 585 594 603 604 606 607 622 627 638 639 645 646 650 652 655 658 664 673 683 685 696 703 704 716 717 731 736 740 743 761 763 771 783 784 795 817 824 832 839 851 853 854 857 859 865 873 875 880 883 884 894 896 903 908 926 930 938 955 966 974 983 988 990 1004 1025 1030 1032 1040 1053 1055 1056 1074 1075 1087 1088 1104 1106 1114 1119 1131 1146 1165 1166 1169 1181 1186 1189 1190 1195 1212 1214 1215 1219 1236 1237 1242 1246 1249 1254 1257 1258 1262 1265 1267 1268 1278 1290 1300 1319 1347 1370 1372 1375 1384 1392 1395 1396 1407 1410 1412 1413 1425 1445 1447 1459 1472 1487 1495 1497 1501 1502 1514 1537 1538 1547 1549 1568 1590 1596 1609 1612 1617 1623 1639 1641 1654 1664 1670 1692 1701 1703 1708 1736 1745 1746 1747 1748 1754 1755 1761 1772 1797 1800 1804 1817 1831 1835 1838 1840 1842 1843 1846 1849 1851 1857 1861 1862 1864 1867 1876

    ## Compute CFA for group 11 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   204 307 1660

    ## Compute CFA for group 12 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   63 139 261 300 481 484 650 652 653 655 658 659 660 661 663 664 672 674 749 1079 1486 1505 1506 1508 1673 1718 1776 2095 2101 2104 2234 2235 2236 2237

    ## Compute CFA for group 13 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   268 341 382 396 499 1134 1355 1358 1513 1627 1792 1793 1935 1936 1937 1942 1964 1965 1966 1974 1980 2040 2043 2070 2082 2086 2098 2147 2155 2278 2303

    ## Compute CFA for group 14 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   188 295 331 429 435 436 439 441 444 445 591 781 810 874 1013 1029 1125 1157 1191 1515

    ## Compute CFA for group 15 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   34 90 93 130 173 188 211 218 219 263 264 323 324 336 386 440 454 467 468 469 475 476 528 530 535 557 563 564 566 616 623 630 692 696 714 724 726 727 729 730 782 783 784 817 822 876 880 882 1006 1015 1021 1044 1053 1055 1056 1060 1063 1116 1164 1166 1187 1322 1345 1347 1348 1385 1387 1394 1395 1447 1448 1449 1461 1482 1484 1493 1507 1515 1532

    ## Compute CFA for group 16 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   129 156 162 164 186 187 188 189 191 192 193 194 195 204 207 208 210 270 298 305 316 335 341 342 343 344 355 367 396 409 425 443 521 541 544 545 546 603 758 764 798 799 800 801 803 804 805 816 825 997 998 1000 1005 1006 1010 1012 1013 1017 1018 1019 1020 1021 1043 1070 1073 1085 1107 1112 1115 1128 1141 1201 1244 1246 1254 1258 1259 1403 1445 1466 1467 1471 1487 1519 1521 1527 1530 1531 1532 1535 1541 1542 1553 1557 1558 1559 1570 1590 1591 1593 1636 1653 1655 1656 1657 1699 1705 1744 1782 1785 1786 1791 1794 1891 1893 1894 1895 1964 1981 1982 1984 1995 2010 2011 2012 2021 2022 2029 2031 2034 2060 2106 2107 2113 2114 2145 2232 2233 2244 2267 2272 2277 2331 2342 2356 2357 2392 2394 2397 2471 2478 2479 2480 2481 2513 2518

    ## Compute CFA for group 17 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   41 59 62 63 64 69 77 78 91 114 136 138 144 151 156 187 244 262 267 274 339 359 380 391 411 464 510 539 548 558 574 591 625 751 765 776 782 796 800 801 815 819 933 948 1108 1139 1171 1182 1367 1375 1463 1789

    ## Compute CFA for group 18 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   150 267 311 351 422 464 536 633 820 830 922 945 963 982 1060 1383 1394 1432 1462 1659 1729 1819 1823

    ## Compute CFA for group 19 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   5 87 95 232 233 333 404 438 835 846 1036 1057 1233 1426

    ## Compute CFA for group 20 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   41 245 312 562 608 817 877 1329 1661 1708

    ## Compute CFA for group 21 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   405

    ## Compute CFA for group 22 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   549 770 773 814 2156 2165

    ## Compute CFA for group 23 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   56 86 103 112 190 285 288 363 414 418 560 636 719 735 846 858 866 1000 1020 1030 1067 1072 1101 1147 1281 1403 1409

    ## Compute CFA for group 24 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   11 23 29 84 91 179 182 203 214 231 310 532 535 546 634 782 991 1123 1129 1131 1170 1275 1353 1391 1394

    ## Compute CFA for group 25 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   39 72 74 111 158 179 180 207 208 212 459 541 608 615 625 640 725 726 727 728 729 747 752 937 972 1070 1071 1105 1215 1326 1368 1469 1519 1595

    ## Compute CFA for group 26 | model 2PM

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   39 179 219 316 330 335 338 361 367 428 460 472 480 486 509 523 527 539 613 630 646 652 670 848 854 868 871 902 971 985 993 1073 1111 1188 1234 1325 1368 1379 1390 1522 1579 1648 1657 1675 1748 1749 1788 1796 1884

``` r
# Fit
config_fit <- lavaan::cfa(
    " f =~ ipmodst + imptrad + ipfrule + ipbhprp ",
    data = traco_dat, group = "country", std.lv = TRUE,
    missing = "ML"
)
```

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   71 1451

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   1940 2433 2677 3038 3048 3401 3440 3652 3774

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   4633 4784 4987 5168 5581 5612 5636

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   6269 6628 6629 6630 6631 6633 6634 6636 6637 6638 6639 6640 6641 6642 6643 6644 6645

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   6777 6779 6941 7014 7032 7044 7092 7176 7255 7466 7473 7485 7496 7603 7610 7721 7728 7733 7735 7760 7819 7901 7912 7965 7971 7982 8025 8041 8067 8086 8216 8268 8348 8388 8506 8525 8599 8675 8697 8742 8881 8927 8929

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   10501 10541 10945 11239 11439 11458 11923 11982 12109 12120

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   12216 12526 12616 12678 12948 12956 12998 13220 13293 13529

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   15908

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   17433 17443 17447 17458 17466 17467 17485 17491 17505 17515 17520 17527 17541 17542 17580 17650 17690 17723 17724 17752 17761 17778 17797 17798 17799 17820 17826 17830 17831 17834 17835 17846 17862 17863 17873 17880 17885 17891 17900 17902 17912 17916 17934 17951 17983 17992 18001 18002 18004 18005 18020 18025 18036 18037 18043 18044 18048 18050 18053 18056 18062 18071 18081 18083 18094 18101 18102 18114 18115 18129 18134 18138 18141 18159 18161 18169 18181 18182 18193 18215 18222 18230 18237 18249 18251 18252 18255 18257 18263 18271 18273 18278 18281 18282 18292 18294 18301 18306 18324 18328 18336 18353 18364 18372 18381 18386 18388 18402 18423 18428 18430 18438 18451 18453 18454 18472 18473 18485 18486 18502 18504 18512 18517 18529 18544 18563 18564 18567 18579 18584 18587 18588 18593 18610 18612 18613 18617 18634 18635 18640 18644 18647 18652 18655 18656 18660 18663 18665 18666 18676 18688 18698 18717 18745 18768 18770 18773 18782 18790 18793 18794 18805 18808 18810 18811 18823 18843 18845 18857 18870 18885 18893 18895 18899 18900 18912 18935 18936 18945 18947 18966 18988 18994 19007 19010 19015 19021 19037 19039 19052 19062 19068 19090 19099 19101 19106 19134 19143 19144 19145 19146 19152 19153 19159 19170 19195 19198 19202 19215 19229 19233 19236 19238 19240 19241 19244 19247 19249 19255 19259 19260 19262 19265 19274

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   19480 19583 20936

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   21067 21143 21265 21304 21485 21488 21654 21656 21657 21659 21662 21663 21664 21665 21667 21668 21676 21678 21753 22083 22490 22509 22510 22512 22677 22722 22780 23099 23105 23108 23238 23239 23240 23241

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   23694 23767 23808 23822 23925 24560 24781 24784 24939 25053 25218 25219 25361 25362 25363 25368 25390 25391 25392 25400 25406 25466 25469 25496 25508 25512 25524 25573 25581 25704 25729

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   26329 26436 26472 26570 26576 26577 26580 26582 26585 26586 26732 26922 26951 27015 27154 27170 27266 27298 27332 27656

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   27824 27880 27883 27920 27963 27978 28001 28008 28009 28053 28054 28113 28114 28126 28176 28230 28244 28257 28258 28259 28265 28266 28318 28320 28325 28347 28353 28354 28356 28406 28413 28420 28482 28486 28504 28514 28516 28517 28519 28520 28572 28573 28574 28607 28612 28666 28670 28672 28796 28805 28811 28834 28843 28845 28846 28850 28853 28906 28954 28956 28977 29112 29135 29137 29138 29175 29177 29184 29185 29237 29238 29239 29251 29272 29274 29283 29297 29305 29322

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   29480 29507 29513 29515 29537 29538 29539 29540 29542 29543 29544 29545 29546 29555 29558 29559 29561 29621 29649 29656 29667 29686 29692 29693 29694 29695 29706 29718 29747 29760 29776 29794 29872 29892 29895 29896 29897 29954 30109 30115 30149 30150 30151 30152 30154 30155 30156 30167 30176 30348 30349 30351 30356 30357 30361 30363 30364 30368 30369 30370 30371 30372 30394 30421 30424 30436 30458 30463 30466 30479 30492 30552 30595 30597 30605 30609 30610 30754 30796 30817 30818 30822 30838 30870 30872 30878 30881 30882 30883 30886 30892 30893 30904 30908 30909 30910 30921 30941 30942 30944 30987 31004 31006 31007 31008 31050 31056 31095 31133 31136 31137 31142 31145 31242 31244 31245 31246 31315 31332 31333 31335 31346 31361 31362 31363 31372 31373 31380 31382 31385 31411 31457 31458 31464 31465 31496 31583 31584 31595 31618 31623 31628 31682 31693 31707 31708 31743 31745 31748 31822 31829 31830 31831 31832 31864 31869

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   31968 31986 31989 31990 31991 31996 32004 32005 32018 32041 32063 32065 32071 32078 32083 32114 32171 32189 32194 32201 32266 32286 32307 32318 32338 32391 32437 32466 32475 32485 32501 32518 32552 32678 32692 32703 32709 32723 32727 32728 32742 32746 32860 32875 33035 33066 33098 33109 33294 33302 33390 33716

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   34371 34488 34532 34572 34643 34685 34757 34854 35041 35051 35143 35166 35184 35203 35281 35604 35615 35653 35683 35880 35950 36040 36044

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   36055 36137 36145 36282 36283 36383 36454 36488 36885 36896 37086 37107 37283 37476

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   37639 37843 37910 38160 38206 38415 38475 38927 39259 39306

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   39754

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   42048 42269 42272 42313 43655 43664

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   44150 44180 44197 44206 44284 44379 44382 44457 44508 44512 44654 44730 44813 44829 44940 44952 44960 45094 45114 45124 45161 45166 45195 45241 45375 45497 45503

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   45602 45614 45620 45675 45682 45770 45773 45794 45805 45822 45901 46123 46126 46137 46225 46373 46582 46714 46720 46722 46761 46866 46944 46982 46985

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   47033 47066 47068 47105 47152 47173 47174 47201 47202 47206 47453 47535 47602 47609 47619 47634 47719 47720 47721 47722 47723 47741 47746 47931 47966 48064 48065 48099 48209 48320 48362 48463 48513 48589

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   48889 49029 49069 49166 49180 49185 49188 49211 49217 49278 49310 49322 49330 49336 49359 49373 49377 49389 49463 49480 49496 49502 49520 49698 49704 49718 49721 49752 49821 49835 49843 49923 49961 50038 50084 50175 50218 50229 50240 50372 50429 50498 50507 50525 50598 50599 50638 50646 50734

``` r
# Correct N by item
real_n <-
    apply(!is.na(traco_dat[c("ipmodst", "imptrad", "ipfrule", "ipbhprp")]),
          MARGIN = 2,
          FUN = \(x) tapply(x, traco_dat$country, FUN = sum))
aligned_res <- invariance.alignment(
    config_res$lambda,
    nu = config_res$nu,
    wgt = matrix(sqrt(real_n),
                 nrow = config_res$G,
                 ncol = config_res$I)
)
# Aligned loadings
aligned_res$lambda.aligned |> round(digits = 4)
```

    ##    ipmodst imptrad ipfrule ipbhprp
    ## 2   0.3935  0.5584  0.6885  0.7626
    ## 3   0.0211  0.0143  0.0186  0.0212
    ## 4   0.0149  0.0157  0.0198  0.0244
    ## 5   0.0188  0.0148  0.0198  0.0221
    ## 6   0.0190  0.0169  0.0171  0.0227
    ## 7   0.0128  0.0160  0.0233  0.0215
    ## 8   0.0167  0.0106  0.0190  0.0257
    ## 9   0.0180  0.0162  0.0198  0.0220
    ## 10  0.0123  0.0202  0.0183  0.0229
    ## 11  0.0144  0.0166  0.0231  0.0205
    ## 12  0.0141  0.0192  0.0160  0.0247
    ## 13  0.0132  0.0150  0.0208  0.0246
    ## 14  0.0189  0.0167  0.0201  0.0205
    ## 15  0.0206  0.0174  0.0180  0.0200
    ## 16  0.0217  0.0168  0.0159  0.0208
    ## 17  0.0181  0.0171  0.0186  0.0224
    ## 18  0.0165  0.0147  0.0218  0.0222
    ## 21  0.0158  0.0170  0.0211  0.0218
    ## 22  0.0150  0.0162  0.0220  0.0219
    ## 23  0.0200  0.0174  0.0181  0.0207
    ## 24  0.0178  0.0205  0.0174  0.0200
    ## 25  0.0203  0.0161  0.0183  0.0211
    ## 26  0.0109  0.0111  0.0218  0.0256
    ## 27  0.0142  0.0214  0.0169  0.0221
    ## 28  0.0189  0.0216  0.0143  0.0196
    ## 30  0.0175  0.0171  0.0190  0.0225

``` r
# Aligned intercepts
aligned_res$nu.aligned |> round(digits = 4)
```

    ##    ipmodst imptrad ipfrule ipbhprp
    ## 2   2.5100  2.6751  3.2598  2.5891
    ## 3   2.8987  2.3618  2.8251  2.5931
    ## 4   2.3912  2.6139  3.2756  2.5873
    ## 5   2.6532  2.0812  3.2781  2.5883
    ## 6   2.9596  2.7813  2.7649  2.5840
    ## 7   2.5233  2.6605  3.0997  2.5893
    ## 8   3.4076  2.6834  2.9403  2.5826
    ## 9   2.7939  2.8428  3.2498  2.5750
    ## 10  2.3834  2.9553  3.2524  2.5779
    ## 11  2.9548  2.8653  2.7743  2.5824
    ## 12  2.2997  3.1471  3.8675  2.5794
    ## 13  2.7396  2.8171  3.2859  2.5739
    ## 14  2.7387  2.3739  3.5892  2.5867
    ## 15  3.0302  2.4275  3.3712  2.5732
    ## 16  2.8109  2.6197  3.6457  2.5782
    ## 17  2.6561  2.7006  3.0007  2.5881
    ## 18  2.5631  2.7995  2.7632  2.5930
    ## 21  3.0813  2.6026  2.6980  2.5944
    ## 22  3.5170  2.9328  3.0564  2.5727
    ## 23  3.0128  2.3873  2.6710  2.5975
    ## 24  2.2788  2.4167  2.8458  2.6050
    ## 25  2.7171  2.4877  3.1317  2.5890
    ## 26  2.6289  2.9013  3.0269  2.5849
    ## 27  2.3368  2.7488  3.2574  2.5856
    ## 28  2.7360  2.6269  2.7144  2.5958
    ## 30  3.2478  2.4033  2.8334  2.5880

``` r
# Uniqueness (unique variances)
lavInspect(config_fit, what = "est") |>
    vapply(FUN = \(x) diag(x$theta), FUN.VALUE = numeric(4)) |>
    t() |>
    round(digits = 4)
```

    ##    ipmodst imptrad ipfrule ipbhprp
    ## 2   1.0257  1.2342  1.1499  0.6479
    ## 3   0.9081  0.9298  0.9915  0.5197
    ## 4   1.0462  1.5830  1.4432  0.8722
    ## 5   0.8173  0.6088  1.3907  0.5692
    ## 6   0.9523  1.3160  0.8890  0.5540
    ## 7   1.3047  1.5904  1.1567  0.9199
    ## 8   1.4677  1.8022  1.1828  0.7625
    ## 9   1.0912  1.5069  1.1254  0.6780
    ## 10  0.9168  1.4625  1.3414  0.5029
    ## 11  1.1457  1.4604  0.7971  0.6652
    ## 12  1.1913  1.9926  1.6224  1.0070
    ## 13  1.3176  1.6296  1.2448  0.5918
    ## 14  0.8749  0.8855  1.5059  0.6773
    ## 15  1.0816  1.0239  1.4651  0.6797
    ## 16  0.8997  1.3091  1.6044  0.7216
    ## 17  0.9976  1.3082  1.1707  0.5685
    ## 18  0.9880  1.7680  0.9979  0.6833
    ## 21  1.2176  1.1826  0.7727  0.6128
    ## 22  1.4452  1.4229  0.8459  0.7416
    ## 23  0.9590  0.7759  0.7406  0.5125
    ## 24  0.8328  0.6166  0.8516  0.5981
    ## 25  0.9241  1.0360  1.2187  0.6684
    ## 26  1.3622  1.8425  1.1641  0.9501
    ## 27  0.6266  0.8301  1.3713  0.5863
    ## 28  0.7376  0.8217  0.8641  0.7128
    ## 30  1.6078  1.1653  1.2498  0.7759

``` r
aligned_res$es.invariance
```

    ##          loadings intercepts
    ## R2     -0.4630238  0.9828959
    ## sqrtU2  1.2095552  0.1307826
    ## rbar    0.4750482  0.4314957

``` r
# fmacs
compute_pvar <- function(x, g, na.rm = TRUE) {
    mean((x - ave(x, g, FUN = \(xx) mean(xx, na.rm = na.rm)))^2,
         na.rm = na.rm)
}
pvar <- apply(
    traco_dat[c("ipmodst", "imptrad", "ipfrule", "ipbhprp")],
    MARGIN = 2,
    FUN = compute_pvar,
    g = traco_dat$country
)
pinsearch::fmacs(
    aligned_res$nu.aligned,
    loadings = aligned_res$lambda.aligned,
    pooled_item_sd = sqrt(pvar),
    num_obs = real_n
)
```

    ##         ipmodst   imptrad   ipfrule   ipbhprp
    ## fmacs 0.2571163 0.1864718 0.2539605 0.1158526

``` r
pinsearch::fmacs(
    aligned_res$nu.aligned,
    loadings = aligned_res$lambda.aligned,
    pooled_item_sd = sqrt(pvar),
    num_obs = real_n,
    group_factor = rep(0:1, each = 13)
)
```

    ##          ipmodst    imptrad   ipfrule    ipbhprp
    ## fmacs 0.04348794 0.02785598 0.0734089 0.02426615

``` r
# Mplus loading R2: 0.248, 0.615, 0.280, 0.954
# Mplus intercept R2: 0.176, 0.000, 0.414, 0.750
```

``` r
# Save results
saveRDS(config_fit, file = "traco_config_fit.rds")
saveRDS(aligned_res, file = "traco_aligned_res.rds")
```
