RProcedureAAPILaGuardiaWithCUNYData2024
================
Tomonori Nagano
2023-12-04

# RProcedureAAPILaGuardiaIPEDS2023

- To analyze the AAPI student, faculty, staff data at LaGuardia
  Community College using data from CUNY’s OAREDA and LaGuardia IR.

## Setting up the environment

``` r
# clear the cache
rm(list = ls())

library(ggplot2); library(gdata); library(ggthemes); library(plyr); 
library(memisc); library(reshape2); library(devtools)
library(dplyr);
library(knitr); 
library(kableExtra); # something is wrong with kableExtra. Had to install directly from GitHub "devtools::install_github("kupietz/kableExtra")"
library(data.table);
#library(devtools); library(ipeds); library(xtable); library(openxlsx); library(foreign); 
#install.packages("acs"); install.packages("ggplot2"); install.packages("maps"); install.packages("mapdata"); 

# turning off scientific notation of numbers
options(scipen=999)

#setwd("~/Desktop/")
setwd("~/Dropbox/Documents/USB/700LaGuardia/eventsOnCampus/AsianHeritageCelebration/AHC2024/")

# change the default width
width.default <- getOption("width"); options(width=120)

# the add comma fonction
addComma<-function(x) {format(x, big.mark = ',', trim = TRUE, scientific = FALSE)}

# creating a notin function
`%notin%` <- Negate(`%in%`)
```

## Analyzing students’ headcounts by race/ethnicity between 1990 and 2022

- The data were obtained from CUNY’s OAREDA (Office of Applied Research,
  Evaluation, and Data Analytics) website
  - <https://public.tableau.com/app/profile/oira.cuny/vizzes>
  - Select “Student Data Book”
    (<https://public.tableau.com/app/profile/oira.cuny/viz/StudentDataBook/Enrollment>)
  - Choose “Enrollment Drilldown”
  - Choose “LaGuardia Community College” using the “College” filter
- Summary
  - The total number of students (headcounts) peaked in 2014, with a
    headcount of 20,231. However, since then, the number has been
    declining, particularly after 2020 (probably due ot the pandemic).
    This decline in student enrollment is reflected among AAPI students,
    as they have also experienced a sharp decrease after 2020.
  - The AAPI student population at LaGuardia has seen the largest
    increase in enrollment over the years. From 1990 to 2022, the number
    of AAPI students rose from 1,406 to 3,298, an increase of 1,892.
    Hispanic students also saw significant growth, with an increase of
    1,772 during the same period.
  - In terms of proportions, AAPI students accounted for 25.2% of the
    total student headcount at LaGuardia in 2022. This proportion
    continued to increase even after 2020. While there was a decline in
    AAPI students after 2020, the rate of decline was not as steep as
    students from other racial/ethnic backgrounds. As a result, AAPI
    students are now more represented than students from other
    racial/ethnic backgrounds in recent years.

``` r
tempData <- read.csv("AHC2024CUNY_OAREDA_StudentDataBook2023.csv", sep = ",")
# change to factors
tempData$Year <- as.factor(tempData$Year)
tempData$Race <- as.factor(tempData$Race)
# create the sum of parttime and fulltime students
tempData$Headcount = tempData$Fulltime + tempData$Partitme
# create a table
tableRace = dcast(tempData, Year ~ Race, value.var = "Headcount")
```

    ## Warning in dcast(tempData, Year ~ Race, value.var = "Headcount"): The dcast generic in data.table has been passed a
    ## data.frame and will attempt to redirect to the reshape2::dcast; please note that reshape2 is deprecated, and this
    ## redirection is now deprecated as well. Please do this redirection yourself like reshape2::dcast(tempData). In the next
    ## version, this warning will become an error.

``` r
# double checking the total is accurate
tableRace$sum = tableRace$White + tableRace$Hispanic + tableRace$Black + tableRace$`Asian or Pacific Islander` + tableRace$`American Indian or Native American`
tableRace[,c("Year", "Total","sum")]
```

    ##    Year Total   sum
    ## 1  1990  9170  9170
    ## 2  1991  9399  9399
    ## 3  1992  9762  9762
    ## 4  1993 10491 10491
    ## 5  1994 11004 11004
    ## 6  1995 10695 10695
    ## 7  1996 11080 11083
    ## 8  1997 10925 10925
    ## 9  1998 11058 11058
    ## 10 1999 11282 11282
    ## 11 2000 11778 11778
    ## 12 2001 11426 11426
    ## 13 2002 12599 12599
    ## 14 2003 12768 12768
    ## 15 2004 13592 13592
    ## 16 2005 13489 13489
    ## 17 2006 14185 14185
    ## 18 2007 15169 15181
    ## 19 2008 15540 15540
    ## 20 2009 17028 17028
    ## 21 2010 17569 17569
    ## 22 2011 18623 18623
    ## 23 2012 19287 19287
    ## 24 2013 19773 19773
    ## 25 2014 20231 20231
    ## 26 2015 19582 19582
    ## 27 2016 19456 19456
    ## 28 2017 19373 19373
    ## 29 2018 19300 19300
    ## 30 2019 18555 18555
    ## 31 2020 16971 16971
    ## 32 2021 14919 14919
    ## 33 2022 13064 13064

``` r
tableRaceNew = tableRace[,c("Year", "White","Hispanic","Black","Asian or Pacific Islander","American Indian or Native American","Total")]

# print the table in HTML
tableRaceNew %>% knitr::kable("html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Year
</th>
<th style="text-align:right;">
White
</th>
<th style="text-align:right;">
Hispanic
</th>
<th style="text-align:right;">
Black
</th>
<th style="text-align:right;">
Asian or Pacific Islander
</th>
<th style="text-align:right;">
American Indian or Native American
</th>
<th style="text-align:right;">
Total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1990
</td>
<td style="text-align:right;">
1741
</td>
<td style="text-align:right;">
3395
</td>
<td style="text-align:right;">
2603
</td>
<td style="text-align:right;">
1406
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
9170
</td>
</tr>
<tr>
<td style="text-align:left;">
1991
</td>
<td style="text-align:right;">
1722
</td>
<td style="text-align:right;">
3600
</td>
<td style="text-align:right;">
2565
</td>
<td style="text-align:right;">
1494
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
9399
</td>
</tr>
<tr>
<td style="text-align:left;">
1992
</td>
<td style="text-align:right;">
1812
</td>
<td style="text-align:right;">
3711
</td>
<td style="text-align:right;">
2635
</td>
<td style="text-align:right;">
1589
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
9762
</td>
</tr>
<tr>
<td style="text-align:left;">
1993
</td>
<td style="text-align:right;">
2014
</td>
<td style="text-align:right;">
4004
</td>
<td style="text-align:right;">
2774
</td>
<td style="text-align:right;">
1681
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
10491
</td>
</tr>
<tr>
<td style="text-align:left;">
1994
</td>
<td style="text-align:right;">
2207
</td>
<td style="text-align:right;">
4227
</td>
<td style="text-align:right;">
2768
</td>
<td style="text-align:right;">
1777
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
11004
</td>
</tr>
<tr>
<td style="text-align:left;">
1995
</td>
<td style="text-align:right;">
2064
</td>
<td style="text-align:right;">
4244
</td>
<td style="text-align:right;">
2658
</td>
<td style="text-align:right;">
1709
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
10695
</td>
</tr>
<tr>
<td style="text-align:left;">
1996
</td>
<td style="text-align:right;">
2115
</td>
<td style="text-align:right;">
4573
</td>
<td style="text-align:right;">
2677
</td>
<td style="text-align:right;">
1704
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
11080
</td>
</tr>
<tr>
<td style="text-align:left;">
1997
</td>
<td style="text-align:right;">
2124
</td>
<td style="text-align:right;">
4465
</td>
<td style="text-align:right;">
2535
</td>
<td style="text-align:right;">
1780
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
10925
</td>
</tr>
<tr>
<td style="text-align:left;">
1998
</td>
<td style="text-align:right;">
2226
</td>
<td style="text-align:right;">
4387
</td>
<td style="text-align:right;">
2458
</td>
<td style="text-align:right;">
1973
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
11058
</td>
</tr>
<tr>
<td style="text-align:left;">
1999
</td>
<td style="text-align:right;">
2221
</td>
<td style="text-align:right;">
4435
</td>
<td style="text-align:right;">
2469
</td>
<td style="text-align:right;">
2138
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
11282
</td>
</tr>
<tr>
<td style="text-align:left;">
2000
</td>
<td style="text-align:right;">
2381
</td>
<td style="text-align:right;">
4558
</td>
<td style="text-align:right;">
2427
</td>
<td style="text-align:right;">
2383
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
11778
</td>
</tr>
<tr>
<td style="text-align:left;">
2001
</td>
<td style="text-align:right;">
2281
</td>
<td style="text-align:right;">
4431
</td>
<td style="text-align:right;">
2246
</td>
<td style="text-align:right;">
2446
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
11426
</td>
</tr>
<tr>
<td style="text-align:left;">
2002
</td>
<td style="text-align:right;">
2578
</td>
<td style="text-align:right;">
4934
</td>
<td style="text-align:right;">
2509
</td>
<td style="text-align:right;">
2563
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
12599
</td>
</tr>
<tr>
<td style="text-align:left;">
2003
</td>
<td style="text-align:right;">
2572
</td>
<td style="text-align:right;">
5089
</td>
<td style="text-align:right;">
2556
</td>
<td style="text-align:right;">
2539
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
12768
</td>
</tr>
<tr>
<td style="text-align:left;">
2004
</td>
<td style="text-align:right;">
2706
</td>
<td style="text-align:right;">
5374
</td>
<td style="text-align:right;">
2852
</td>
<td style="text-align:right;">
2643
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
13592
</td>
</tr>
<tr>
<td style="text-align:left;">
2005
</td>
<td style="text-align:right;">
2561
</td>
<td style="text-align:right;">
5232
</td>
<td style="text-align:right;">
3007
</td>
<td style="text-align:right;">
2664
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
13489
</td>
</tr>
<tr>
<td style="text-align:left;">
2006
</td>
<td style="text-align:right;">
2583
</td>
<td style="text-align:right;">
5419
</td>
<td style="text-align:right;">
3119
</td>
<td style="text-align:right;">
3041
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
14185
</td>
</tr>
<tr>
<td style="text-align:left;">
2007
</td>
<td style="text-align:right;">
2546
</td>
<td style="text-align:right;">
6032
</td>
<td style="text-align:right;">
3142
</td>
<td style="text-align:right;">
3412
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
15169
</td>
</tr>
<tr>
<td style="text-align:left;">
2008
</td>
<td style="text-align:right;">
2533
</td>
<td style="text-align:right;">
6143
</td>
<td style="text-align:right;">
3063
</td>
<td style="text-align:right;">
3771
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
15540
</td>
</tr>
<tr>
<td style="text-align:left;">
2009
</td>
<td style="text-align:right;">
2763
</td>
<td style="text-align:right;">
6857
</td>
<td style="text-align:right;">
3210
</td>
<td style="text-align:right;">
4148
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
17028
</td>
</tr>
<tr>
<td style="text-align:left;">
2010
</td>
<td style="text-align:right;">
2865
</td>
<td style="text-align:right;">
7223
</td>
<td style="text-align:right;">
3350
</td>
<td style="text-align:right;">
4067
</td>
<td style="text-align:right;">
64
</td>
<td style="text-align:right;">
17569
</td>
</tr>
<tr>
<td style="text-align:left;">
2011
</td>
<td style="text-align:right;">
2983
</td>
<td style="text-align:right;">
7554
</td>
<td style="text-align:right;">
3637
</td>
<td style="text-align:right;">
4374
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
18623
</td>
</tr>
<tr>
<td style="text-align:left;">
2012
</td>
<td style="text-align:right;">
3118
</td>
<td style="text-align:right;">
7964
</td>
<td style="text-align:right;">
3845
</td>
<td style="text-align:right;">
4300
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
19287
</td>
</tr>
<tr>
<td style="text-align:left;">
2013
</td>
<td style="text-align:right;">
2995
</td>
<td style="text-align:right;">
8402
</td>
<td style="text-align:right;">
4056
</td>
<td style="text-align:right;">
4245
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
19773
</td>
</tr>
<tr>
<td style="text-align:left;">
2014
</td>
<td style="text-align:right;">
2928
</td>
<td style="text-align:right;">
8611
</td>
<td style="text-align:right;">
4305
</td>
<td style="text-align:right;">
4315
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
20231
</td>
</tr>
<tr>
<td style="text-align:left;">
2015
</td>
<td style="text-align:right;">
2730
</td>
<td style="text-align:right;">
8537
</td>
<td style="text-align:right;">
4036
</td>
<td style="text-align:right;">
4211
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
19582
</td>
</tr>
<tr>
<td style="text-align:left;">
2016
</td>
<td style="text-align:right;">
2623
</td>
<td style="text-align:right;">
8602
</td>
<td style="text-align:right;">
3809
</td>
<td style="text-align:right;">
4353
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
19456
</td>
</tr>
<tr>
<td style="text-align:left;">
2017
</td>
<td style="text-align:right;">
2599
</td>
<td style="text-align:right;">
8496
</td>
<td style="text-align:right;">
3594
</td>
<td style="text-align:right;">
4616
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
19373
</td>
</tr>
<tr>
<td style="text-align:left;">
2018
</td>
<td style="text-align:right;">
2683
</td>
<td style="text-align:right;">
8180
</td>
<td style="text-align:right;">
3667
</td>
<td style="text-align:right;">
4699
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
19300
</td>
</tr>
<tr>
<td style="text-align:left;">
2019
</td>
<td style="text-align:right;">
2638
</td>
<td style="text-align:right;">
7705
</td>
<td style="text-align:right;">
3598
</td>
<td style="text-align:right;">
4536
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
18555
</td>
</tr>
<tr>
<td style="text-align:left;">
2020
</td>
<td style="text-align:right;">
2394
</td>
<td style="text-align:right;">
6862
</td>
<td style="text-align:right;">
3568
</td>
<td style="text-align:right;">
4073
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
16971
</td>
</tr>
<tr>
<td style="text-align:left;">
2021
</td>
<td style="text-align:right;">
2101
</td>
<td style="text-align:right;">
6014
</td>
<td style="text-align:right;">
3285
</td>
<td style="text-align:right;">
3467
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
14919
</td>
</tr>
<tr>
<td style="text-align:left;">
2022
</td>
<td style="text-align:right;">
1790
</td>
<td style="text-align:right;">
5167
</td>
<td style="text-align:right;">
2754
</td>
<td style="text-align:right;">
3298
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
13064
</td>
</tr>
</tbody>
</table>

``` r
# calculating the proportion table
tableRaceProportion = cbind(levels(tableRaceNew[,"Year"]),format(prop.table(data.matrix(tableRaceNew[,c("White","Hispanic","Black","Asian or Pacific Islander","American Indian or Native American")]),margin = 1)*100, digits=1))
tableRaceProportion = as.data.table(tableRaceProportion)
tableRaceProportion %>% knitr::kable("html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
V1
</th>
<th style="text-align:left;">
White
</th>
<th style="text-align:left;">
Hispanic
</th>
<th style="text-align:left;">
Black
</th>
<th style="text-align:left;">
Asian or Pacific Islander
</th>
<th style="text-align:left;">
American Indian or Native American
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1990
</td>
<td style="text-align:left;">
18.99
</td>
<td style="text-align:left;">
37.02
</td>
<td style="text-align:left;">
28.39
</td>
<td style="text-align:left;">
15.33
</td>
<td style="text-align:left;">
0.27
</td>
</tr>
<tr>
<td style="text-align:left;">
1991
</td>
<td style="text-align:left;">
18.32
</td>
<td style="text-align:left;">
38.30
</td>
<td style="text-align:left;">
27.29
</td>
<td style="text-align:left;">
15.90
</td>
<td style="text-align:left;">
0.19
</td>
</tr>
<tr>
<td style="text-align:left;">
1992
</td>
<td style="text-align:left;">
18.56
</td>
<td style="text-align:left;">
38.01
</td>
<td style="text-align:left;">
26.99
</td>
<td style="text-align:left;">
16.28
</td>
<td style="text-align:left;">
0.15
</td>
</tr>
<tr>
<td style="text-align:left;">
1993
</td>
<td style="text-align:left;">
19.20
</td>
<td style="text-align:left;">
38.17
</td>
<td style="text-align:left;">
26.44
</td>
<td style="text-align:left;">
16.02
</td>
<td style="text-align:left;">
0.17
</td>
</tr>
<tr>
<td style="text-align:left;">
1994
</td>
<td style="text-align:left;">
20.06
</td>
<td style="text-align:left;">
38.41
</td>
<td style="text-align:left;">
25.15
</td>
<td style="text-align:left;">
16.15
</td>
<td style="text-align:left;">
0.23
</td>
</tr>
<tr>
<td style="text-align:left;">
1995
</td>
<td style="text-align:left;">
19.30
</td>
<td style="text-align:left;">
39.68
</td>
<td style="text-align:left;">
24.85
</td>
<td style="text-align:left;">
15.98
</td>
<td style="text-align:left;">
0.19
</td>
</tr>
<tr>
<td style="text-align:left;">
1996
</td>
<td style="text-align:left;">
19.08
</td>
<td style="text-align:left;">
41.26
</td>
<td style="text-align:left;">
24.15
</td>
<td style="text-align:left;">
15.37
</td>
<td style="text-align:left;">
0.13
</td>
</tr>
<tr>
<td style="text-align:left;">
1997
</td>
<td style="text-align:left;">
19.44
</td>
<td style="text-align:left;">
40.87
</td>
<td style="text-align:left;">
23.20
</td>
<td style="text-align:left;">
16.29
</td>
<td style="text-align:left;">
0.19
</td>
</tr>
<tr>
<td style="text-align:left;">
1998
</td>
<td style="text-align:left;">
20.13
</td>
<td style="text-align:left;">
39.67
</td>
<td style="text-align:left;">
22.23
</td>
<td style="text-align:left;">
17.84
</td>
<td style="text-align:left;">
0.13
</td>
</tr>
<tr>
<td style="text-align:left;">
1999
</td>
<td style="text-align:left;">
19.69
</td>
<td style="text-align:left;">
39.31
</td>
<td style="text-align:left;">
21.88
</td>
<td style="text-align:left;">
18.95
</td>
<td style="text-align:left;">
0.17
</td>
</tr>
<tr>
<td style="text-align:left;">
2000
</td>
<td style="text-align:left;">
20.22
</td>
<td style="text-align:left;">
38.70
</td>
<td style="text-align:left;">
20.61
</td>
<td style="text-align:left;">
20.23
</td>
<td style="text-align:left;">
0.25
</td>
</tr>
<tr>
<td style="text-align:left;">
2001
</td>
<td style="text-align:left;">
19.96
</td>
<td style="text-align:left;">
38.78
</td>
<td style="text-align:left;">
19.66
</td>
<td style="text-align:left;">
21.41
</td>
<td style="text-align:left;">
0.19
</td>
</tr>
<tr>
<td style="text-align:left;">
2002
</td>
<td style="text-align:left;">
20.46
</td>
<td style="text-align:left;">
39.16
</td>
<td style="text-align:left;">
19.91
</td>
<td style="text-align:left;">
20.34
</td>
<td style="text-align:left;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left;">
2003
</td>
<td style="text-align:left;">
20.14
</td>
<td style="text-align:left;">
39.86
</td>
<td style="text-align:left;">
20.02
</td>
<td style="text-align:left;">
19.89
</td>
<td style="text-align:left;">
0.09
</td>
</tr>
<tr>
<td style="text-align:left;">
2004
</td>
<td style="text-align:left;">
19.91
</td>
<td style="text-align:left;">
39.54
</td>
<td style="text-align:left;">
20.98
</td>
<td style="text-align:left;">
19.45
</td>
<td style="text-align:left;">
0.13
</td>
</tr>
<tr>
<td style="text-align:left;">
2005
</td>
<td style="text-align:left;">
18.99
</td>
<td style="text-align:left;">
38.79
</td>
<td style="text-align:left;">
22.29
</td>
<td style="text-align:left;">
19.75
</td>
<td style="text-align:left;">
0.19
</td>
</tr>
<tr>
<td style="text-align:left;">
2006
</td>
<td style="text-align:left;">
18.21
</td>
<td style="text-align:left;">
38.20
</td>
<td style="text-align:left;">
21.99
</td>
<td style="text-align:left;">
21.44
</td>
<td style="text-align:left;">
0.16
</td>
</tr>
<tr>
<td style="text-align:left;">
2007
</td>
<td style="text-align:left;">
16.77
</td>
<td style="text-align:left;">
39.73
</td>
<td style="text-align:left;">
20.70
</td>
<td style="text-align:left;">
22.48
</td>
<td style="text-align:left;">
0.32
</td>
</tr>
<tr>
<td style="text-align:left;">
2008
</td>
<td style="text-align:left;">
16.30
</td>
<td style="text-align:left;">
39.53
</td>
<td style="text-align:left;">
19.71
</td>
<td style="text-align:left;">
24.27
</td>
<td style="text-align:left;">
0.19
</td>
</tr>
<tr>
<td style="text-align:left;">
2009
</td>
<td style="text-align:left;">
16.23
</td>
<td style="text-align:left;">
40.27
</td>
<td style="text-align:left;">
18.85
</td>
<td style="text-align:left;">
24.36
</td>
<td style="text-align:left;">
0.29
</td>
</tr>
<tr>
<td style="text-align:left;">
2010
</td>
<td style="text-align:left;">
16.31
</td>
<td style="text-align:left;">
41.11
</td>
<td style="text-align:left;">
19.07
</td>
<td style="text-align:left;">
23.15
</td>
<td style="text-align:left;">
0.36
</td>
</tr>
<tr>
<td style="text-align:left;">
2011
</td>
<td style="text-align:left;">
16.02
</td>
<td style="text-align:left;">
40.56
</td>
<td style="text-align:left;">
19.53
</td>
<td style="text-align:left;">
23.49
</td>
<td style="text-align:left;">
0.40
</td>
</tr>
<tr>
<td style="text-align:left;">
2012
</td>
<td style="text-align:left;">
16.17
</td>
<td style="text-align:left;">
41.29
</td>
<td style="text-align:left;">
19.94
</td>
<td style="text-align:left;">
22.29
</td>
<td style="text-align:left;">
0.31
</td>
</tr>
<tr>
<td style="text-align:left;">
2013
</td>
<td style="text-align:left;">
15.15
</td>
<td style="text-align:left;">
42.49
</td>
<td style="text-align:left;">
20.51
</td>
<td style="text-align:left;">
21.47
</td>
<td style="text-align:left;">
0.38
</td>
</tr>
<tr>
<td style="text-align:left;">
2014
</td>
<td style="text-align:left;">
14.47
</td>
<td style="text-align:left;">
42.56
</td>
<td style="text-align:left;">
21.28
</td>
<td style="text-align:left;">
21.33
</td>
<td style="text-align:left;">
0.36
</td>
</tr>
<tr>
<td style="text-align:left;">
2015
</td>
<td style="text-align:left;">
13.94
</td>
<td style="text-align:left;">
43.60
</td>
<td style="text-align:left;">
20.61
</td>
<td style="text-align:left;">
21.50
</td>
<td style="text-align:left;">
0.35
</td>
</tr>
<tr>
<td style="text-align:left;">
2016
</td>
<td style="text-align:left;">
13.48
</td>
<td style="text-align:left;">
44.21
</td>
<td style="text-align:left;">
19.58
</td>
<td style="text-align:left;">
22.37
</td>
<td style="text-align:left;">
0.35
</td>
</tr>
<tr>
<td style="text-align:left;">
2017
</td>
<td style="text-align:left;">
13.42
</td>
<td style="text-align:left;">
43.85
</td>
<td style="text-align:left;">
18.55
</td>
<td style="text-align:left;">
23.83
</td>
<td style="text-align:left;">
0.35
</td>
</tr>
<tr>
<td style="text-align:left;">
2018
</td>
<td style="text-align:left;">
13.90
</td>
<td style="text-align:left;">
42.38
</td>
<td style="text-align:left;">
19.00
</td>
<td style="text-align:left;">
24.35
</td>
<td style="text-align:left;">
0.37
</td>
</tr>
<tr>
<td style="text-align:left;">
2019
</td>
<td style="text-align:left;">
14.22
</td>
<td style="text-align:left;">
41.53
</td>
<td style="text-align:left;">
19.39
</td>
<td style="text-align:left;">
24.45
</td>
<td style="text-align:left;">
0.42
</td>
</tr>
<tr>
<td style="text-align:left;">
2020
</td>
<td style="text-align:left;">
14.11
</td>
<td style="text-align:left;">
40.43
</td>
<td style="text-align:left;">
21.02
</td>
<td style="text-align:left;">
24.00
</td>
<td style="text-align:left;">
0.44
</td>
</tr>
<tr>
<td style="text-align:left;">
2021
</td>
<td style="text-align:left;">
14.08
</td>
<td style="text-align:left;">
40.31
</td>
<td style="text-align:left;">
22.02
</td>
<td style="text-align:left;">
23.24
</td>
<td style="text-align:left;">
0.35
</td>
</tr>
<tr>
<td style="text-align:left;">
2022
</td>
<td style="text-align:left;">
13.70
</td>
<td style="text-align:left;">
39.55
</td>
<td style="text-align:left;">
21.08
</td>
<td style="text-align:left;">
25.24
</td>
<td style="text-align:left;">
0.42
</td>
</tr>
</tbody>
</table>

## Analyzing faculty and staff’s racial/ethnic diversity at LaGuardia Community College between 2020 and 2022

- The data were obtained from CUNY’s Office of Recruitment and Diversity
  (ORD) website
  - <https://www.cuny.edu/about/administration/offices/hr/recruitment-diversity/statistics-and-reports/>
  - See the “2020 to 2022 Three-Year Comparison of CUNY Workforce
    Demographics” report at
    <https://www.cuny.edu/wp-content/uploads/sites/4/media-assets/3-Year-Comparison-2020-to-2022-CUNY-Workforce-Demographics-1.pdf>
  - LaGuardia’s data are on page 45
  - Three different groups are analyzed
    - Instructional staff: Full-time facutly and other instructional
      staff, including substitutes, visiting titles and acting
      appointments.
    - Faculty: Full-time facutly
    - HEO: Full-time HEO titles (a.k.a., staff)
- Summary
  - The representation of AAPI instructional staff in 2022 was only
    16.6%, which slightly decreased from 16.9% in 2020. However, the
    AAPI student body at LaGuardia increased from 24.0% in 2020 to 25.5%
    in 2022.
  - When looking at faculty data, which excludes full-time instructional
    employees without a faculty appointment, the representation of AAPI
    increases slightly to 18.4%. This is largely due to a lower
    representation of Hispanic instructional employees with faculty
    appointments, at just 13.1%.
  - In terms of HEO titles (staff), only 14.2% of HEO titles were
    occupied by AAPI individuals in 2022. Again, the representation of
    AAPI staff has slightly decreased since 2020 when it was 15.2%. In
    contrast, other minority groups such as Hispanic (33%) and Black
    (25.4%) are better represented among the HEO titles.

``` r
tempData <- read.csv("AHC2024CUNY_ORDInstructionalStaff.csv", skip = 0, sep = ",")

tempData2 <- reshape2::melt(tempData, id.vars = c("Group", "Attribute"))
levels(tempData2$variable) <- c("2020","2021","2022")
tempData2$Group <- as.factor(tempData2$Group)
tempData2$Attribute <- as.factor(tempData2$Attribute)
names(tempData2) <- c("Group","Attribute","Year","Count")
```

- Analyzing Instructional Staff (Fulltime only) data

``` r
tempData3 <- drop.levels(tempData2[tempData2$Group=="Instrucitonal Staff",],reorder=FALSE)
tableRace.Inst = reshape2::dcast(tempData3, Year ~ Attribute, value.var = "Count")

# double checking the total is accurate
tableRace.Inst$sum = tableRace.Inst$White + tableRace.Inst$Hispanic + tableRace.Inst$Black + tableRace.Inst$`Asian or Pacific Islander` + tableRace.Inst$`American Indian or Native American` + tableRace.Inst$`Italian American` + tableRace.Inst$`Two or More Races`
tableRace.Inst[,c("Year", "sum")]
```

    ##   Year sum
    ## 1 2020 791
    ## 2 2021 745
    ## 3 2022 736

``` r
tableRaceNew.Inst = tableRace.Inst[,c("Year", "White","Hispanic","Black","Asian or Pacific Islander","American Indian or Native American","Italian American","Two or More Races","sum")]

# print the table in HTML
tableRaceNew.Inst %>% knitr::kable("html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Year
</th>
<th style="text-align:right;">
White
</th>
<th style="text-align:right;">
Hispanic
</th>
<th style="text-align:right;">
Black
</th>
<th style="text-align:right;">
Asian or Pacific Islander
</th>
<th style="text-align:right;">
American Indian or Native American
</th>
<th style="text-align:right;">
Italian American
</th>
<th style="text-align:right;">
Two or More Races
</th>
<th style="text-align:right;">
sum
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2020
</td>
<td style="text-align:right;">
302
</td>
<td style="text-align:right;">
161
</td>
<td style="text-align:right;">
153
</td>
<td style="text-align:right;">
134
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
791
</td>
</tr>
<tr>
<td style="text-align:left;">
2021
</td>
<td style="text-align:right;">
279
</td>
<td style="text-align:right;">
159
</td>
<td style="text-align:right;">
140
</td>
<td style="text-align:right;">
128
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
745
</td>
</tr>
<tr>
<td style="text-align:left;">
2022
</td>
<td style="text-align:right;">
272
</td>
<td style="text-align:right;">
169
</td>
<td style="text-align:right;">
134
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
736
</td>
</tr>
</tbody>
</table>

``` r
# calculating the proportion table
tableRaceProportion.Inst = cbind(levels(tableRaceNew.Inst[,"Year"]),format(prop.table(data.matrix(tableRaceNew.Inst[,c("White","Hispanic","Black","Asian or Pacific Islander","American Indian or Native American","Italian American","Two or More Races")]),margin = 1)*100, digits=1))
tableRaceProportion.Inst = as.data.table(tableRaceProportion.Inst)
tableRaceProportion.Inst %>% knitr::kable("html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
V1
</th>
<th style="text-align:left;">
White
</th>
<th style="text-align:left;">
Hispanic
</th>
<th style="text-align:left;">
Black
</th>
<th style="text-align:left;">
Asian or Pacific Islander
</th>
<th style="text-align:left;">
American Indian or Native American
</th>
<th style="text-align:left;">
Italian American
</th>
<th style="text-align:left;">
Two or More Races
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2020
</td>
<td style="text-align:left;">
38.2
</td>
<td style="text-align:left;">
20.4
</td>
<td style="text-align:left;">
19.3
</td>
<td style="text-align:left;">
16.9
</td>
<td style="text-align:left;">
0.0
</td>
<td style="text-align:left;">
4.6
</td>
<td style="text-align:left;">
0.6
</td>
</tr>
<tr>
<td style="text-align:left;">
2021
</td>
<td style="text-align:left;">
37.4
</td>
<td style="text-align:left;">
21.3
</td>
<td style="text-align:left;">
18.8
</td>
<td style="text-align:left;">
17.2
</td>
<td style="text-align:left;">
0.0
</td>
<td style="text-align:left;">
4.6
</td>
<td style="text-align:left;">
0.7
</td>
</tr>
<tr>
<td style="text-align:left;">
2022
</td>
<td style="text-align:left;">
37.0
</td>
<td style="text-align:left;">
23.0
</td>
<td style="text-align:left;">
18.2
</td>
<td style="text-align:left;">
16.6
</td>
<td style="text-align:left;">
0.0
</td>
<td style="text-align:left;">
4.8
</td>
<td style="text-align:left;">
0.5
</td>
</tr>
</tbody>
</table>

- Analyzing Faculty (Fulltime only) data

``` r
tempData3 <- drop.levels(tempData2[tempData2$Group=="Faculty",],reorder=FALSE)
tableRace.Fac = reshape2::dcast(tempData3, Year ~ Attribute, value.var = "Count")

# double checking the total is accurate
tableRace.Fac$sum = tableRace.Fac$White + tableRace.Fac$Hispanic + tableRace.Fac$Black + tableRace.Fac$`Asian or Pacific Islander` + tableRace.Fac$`American Indian or Native American` + tableRace.Fac$`Italian American` + tableRace.Fac$`Two or More Races`
tableRace.Fac[,c("Year", "sum")]
```

    ##   Year sum
    ## 1 2020 383
    ## 2 2021 362
    ## 3 2022 375

``` r
tableRaceNew.Fac = tableRace.Fac[,c("Year", "White","Hispanic","Black","Asian or Pacific Islander","American Indian or Native American","Italian American","Two or More Races","sum")]

# print the table in HTML
tableRaceNew.Fac %>% knitr::kable("html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Year
</th>
<th style="text-align:right;">
White
</th>
<th style="text-align:right;">
Hispanic
</th>
<th style="text-align:right;">
Black
</th>
<th style="text-align:right;">
Asian or Pacific Islander
</th>
<th style="text-align:right;">
American Indian or Native American
</th>
<th style="text-align:right;">
Italian American
</th>
<th style="text-align:right;">
Two or More Races
</th>
<th style="text-align:right;">
sum
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2020
</td>
<td style="text-align:right;">
196
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
383
</td>
</tr>
<tr>
<td style="text-align:left;">
2021
</td>
<td style="text-align:right;">
181
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
362
</td>
</tr>
<tr>
<td style="text-align:left;">
2022
</td>
<td style="text-align:right;">
188
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
375
</td>
</tr>
</tbody>
</table>

``` r
# calculating the proportion table
tableRaceProportion.Fac = cbind(levels(tableRaceNew.Fac[,"Year"]),format(prop.table(data.matrix(tableRaceNew.Fac[,c("White","Hispanic","Black","Asian or Pacific Islander","American Indian or Native American","Italian American","Two or More Races")]),margin = 1)*100, digits=1))
tableRaceProportion.Fac = as.data.table(tableRaceProportion.Fac)
tableRaceProportion.Fac %>% knitr::kable("html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
V1
</th>
<th style="text-align:left;">
White
</th>
<th style="text-align:left;">
Hispanic
</th>
<th style="text-align:left;">
Black
</th>
<th style="text-align:left;">
Asian or Pacific Islander
</th>
<th style="text-align:left;">
American Indian or Native American
</th>
<th style="text-align:left;">
Italian American
</th>
<th style="text-align:left;">
Two or More Races
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2020
</td>
<td style="text-align:left;">
51.2
</td>
<td style="text-align:left;">
11.2
</td>
<td style="text-align:left;">
13.6
</td>
<td style="text-align:left;">
18.5
</td>
<td style="text-align:left;">
0.0
</td>
<td style="text-align:left;">
5.0
</td>
<td style="text-align:left;">
0.5
</td>
</tr>
<tr>
<td style="text-align:left;">
2021
</td>
<td style="text-align:left;">
50.0
</td>
<td style="text-align:left;">
11.6
</td>
<td style="text-align:left;">
14.1
</td>
<td style="text-align:left;">
19.1
</td>
<td style="text-align:left;">
0.0
</td>
<td style="text-align:left;">
5.0
</td>
<td style="text-align:left;">
0.3
</td>
</tr>
<tr>
<td style="text-align:left;">
2022
</td>
<td style="text-align:left;">
50.1
</td>
<td style="text-align:left;">
13.1
</td>
<td style="text-align:left;">
13.1
</td>
<td style="text-align:left;">
18.4
</td>
<td style="text-align:left;">
0.0
</td>
<td style="text-align:left;">
5.1
</td>
<td style="text-align:left;">
0.3
</td>
</tr>
</tbody>
</table>

- Analyzing HEO (Fulltime only) data

``` r
tempData3 <- drop.levels(tempData2[tempData2$Group=="HEO",],reorder=FALSE)
tableRace.HEO = reshape2::dcast(tempData3, Year ~ Attribute, value.var = "Count")

# double checking the total is accurate
tableRace.HEO$sum = tableRace.HEO$White + tableRace.HEO$Hispanic + tableRace.HEO$Black + tableRace.HEO$`Asian or Pacific Islander` + tableRace.HEO$`American Indian or Native American` + tableRace.HEO$`Italian American` + tableRace.HEO$`Two or More Races`
tableRace.HEO[,c("Year", "sum")]
```

    ##   Year sum
    ## 1 2020 348
    ## 2 2021 324
    ## 3 2022 303

``` r
tableRaceNew.HEO = tableRace.HEO[,c("Year", "White","Hispanic","Black","Asian or Pacific Islander","American Indian or Native American","Italian American","Two or More Races","sum")]

# print the table in HTML
tableRaceNew.HEO %>% knitr::kable("html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Year
</th>
<th style="text-align:right;">
White
</th>
<th style="text-align:right;">
Hispanic
</th>
<th style="text-align:right;">
Black
</th>
<th style="text-align:right;">
Asian or Pacific Islander
</th>
<th style="text-align:right;">
American Indian or Native American
</th>
<th style="text-align:right;">
Italian American
</th>
<th style="text-align:right;">
Two or More Races
</th>
<th style="text-align:right;">
sum
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2020
</td>
<td style="text-align:right;">
87
</td>
<td style="text-align:right;">
102
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
348
</td>
</tr>
<tr>
<td style="text-align:left;">
2021
</td>
<td style="text-align:right;">
81
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
324
</td>
</tr>
<tr>
<td style="text-align:left;">
2022
</td>
<td style="text-align:right;">
70
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
303
</td>
</tr>
</tbody>
</table>

``` r
# calculating the proportion table
tableRaceProportion.HEO = cbind(levels(tableRaceNew.HEO[,"Year"]),format(prop.table(data.matrix(tableRaceNew.HEO[,c("White","Hispanic","Black","Asian or Pacific Islander","American Indian or Native American","Italian American","Two or More Races")]),margin = 1)*100, digits=1))
tableRaceProportion.HEO = as.data.table(tableRaceProportion.HEO)
tableRaceProportion.HEO %>% knitr::kable("html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
V1
</th>
<th style="text-align:left;">
White
</th>
<th style="text-align:left;">
Hispanic
</th>
<th style="text-align:left;">
Black
</th>
<th style="text-align:left;">
Asian or Pacific Islander
</th>
<th style="text-align:left;">
American Indian or Native American
</th>
<th style="text-align:left;">
Italian American
</th>
<th style="text-align:left;">
Two or More Races
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2020
</td>
<td style="text-align:left;">
25.0
</td>
<td style="text-align:left;">
29.3
</td>
<td style="text-align:left;">
26.1
</td>
<td style="text-align:left;">
15.2
</td>
<td style="text-align:left;">
0.0
</td>
<td style="text-align:left;">
3.4
</td>
<td style="text-align:left;">
0.9
</td>
</tr>
<tr>
<td style="text-align:left;">
2021
</td>
<td style="text-align:left;">
25.0
</td>
<td style="text-align:left;">
30.9
</td>
<td style="text-align:left;">
24.4
</td>
<td style="text-align:left;">
15.1
</td>
<td style="text-align:left;">
0.0
</td>
<td style="text-align:left;">
3.4
</td>
<td style="text-align:left;">
1.2
</td>
</tr>
<tr>
<td style="text-align:left;">
2022
</td>
<td style="text-align:left;">
23.1
</td>
<td style="text-align:left;">
33.0
</td>
<td style="text-align:left;">
25.4
</td>
<td style="text-align:left;">
14.2
</td>
<td style="text-align:left;">
0.0
</td>
<td style="text-align:left;">
3.6
</td>
<td style="text-align:left;">
0.7
</td>
</tr>
</tbody>
</table>
