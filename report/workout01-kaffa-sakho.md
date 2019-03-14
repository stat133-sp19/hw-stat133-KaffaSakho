workout01-kaffa-sakho
================

5.1) Effective Shooting Percentage
==================================

Importing the Shots Data
========================

``` r
getwd()
```

    ## [1] "/Users/AKaffa/Desktop/workout01/report"

``` r
setwd("/Users/AKaffa/Desktop/workout01")
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
esp <- read.csv("data/shots-data.csv", header = TRUE, row.names=1)
```

2PT Effective Shooting % by Player:
===================================

``` r
esp_2pt <- summarise(group_by(filter(esp, shot_type == "2PT Field Goal"), name), total = n(), made=length(shot_made_flag[shot_made_flag == "shot_yes"]))
esp_2pt <- mutate(esp_2pt, perc_made= made/total)
esp_2pt <- arrange(esp_2pt, desc(perc_made))
```

3PT Effective Shooting % by Player:
===================================

``` r
esp_3pt <- summarise(group_by(filter(esp, shot_type == "3PT Field Goal"), name), total = n(), made=length(shot_made_flag[shot_made_flag == "shot_yes"]))
esp_3pt <- mutate(esp_3pt, perc_made= made/total)
esp_3pt <- arrange(esp_3pt, desc(perc_made))
```

Overall Effective Shooting % by Player:
=======================================

``` r
esp_overall <- summarise(group_by(esp, name), total = n(), made=length(shot_made_flag[shot_made_flag == "shot_yes"]))
esp_overall <- mutate(esp_overall, perc_made= made/total)
esp_overall <- arrange(esp_overall, desc(perc_made))
```

Introduction/Motivation
=======================

![](https://clutchpoints.com/wp-content/uploads/2017/01/Stephen-Curry-Draymond-Green-Kevin-Durant-Klay-Thompson-Andre-Iguodala-warriors-1024x532.jpg) The Strength in the Numbers

The 2016-17 NBA season was undoubtedly the most dominant season for the Golden State Warriors. According to Fox Sports, following the 2016-17 season it was high time to see the Warriors as the greatest team of all time. The season was particularly the stage for five Warriors players to shine with their unprecedented shot statistics: the newly joined Kevin Durant, Draymond Green, Klay Thompson, Stephen Curry and Andre Iguodala. The purpose of this report is to analyze the shot statistics of these five players as a way to understand how they led GSW to its championship title. Demystifying shot statistics of the top Warriors players does not only help acknowledge their remarkable performance but also helps identify their strength and areas of growth in order to understand how the greatest of all time has gained and plan to keep its title.

Background
==========

Since Kevin Durant joined the warriors in the 2016 season, the quintuple: Curry, Iguodala, Thompson, Durant and Green make the usual starting lineup; they have contributed greatly in the success of the warriors this season. Steph Curry is the point-guard prodigee for the Warriors, he is known for his great 3pts record. Andre Iguodala is a shooting guard and small forward for GSW; he is known as the defensive swingman of the team. Klay Thompson is a shoot guard for the warriors and splash brother of Curry, together they make the most fearful pair when it comes to 3pt field goals. Finally, Draymond Green is the power forward of the warriors and won the NBA Defensive Player of the Year in 2016. One of the main goals of this article is to show that the warriors famous "Strength in Numbers" slogan lies in the diversity of strengths in shots across its 5 starters (e.g consistency in shots across periods, distance of successful shots, percentage of successful shots, etc).

Data & Analysis
===============

The summary of the aggregated shot statistics of all 5 players attest of the immense capacity of the Warriors to shoot long shots with an average shot distance of 15.58 inches which is greater than the NBA average shot distance of the 2015-2016 season (12.86 inches).

``` r
setwd("/Users/AKaffa/Desktop/workout01")
esp <- read.csv("data/shots-data.csv", header = TRUE, row.names=1)
(summary(esp))
```

    ##                  team_name       game_date        season    
    ##  Golden State Warriors:4334   10/28/16:  76   Min.   :2016  
    ##                               12/13/16:  75   1st Qu.:2016  
    ##                               11/7/16 :  74   Median :2016  
    ##                               1/20/17 :  73   Mean   :2016  
    ##                               11/16/16:  72   3rd Qu.:2016  
    ##                               11/18/16:  72   Max.   :2016  
    ##                               (Other) :3892                 
    ##      period      minutes_remaining seconds_remaining  shot_made_flag
    ##  Min.   :1.000   Min.   : 0.000    Min.   : 0.00     shot_no :2243  
    ##  1st Qu.:1.000   1st Qu.: 2.000    1st Qu.:13.00     shot_yes:2091  
    ##  Median :2.000   Median : 5.000    Median :29.00                    
    ##  Mean   :2.351   Mean   : 5.412    Mean   :28.60                    
    ##  3rd Qu.:3.000   3rd Qu.: 8.000    3rd Qu.:43.75                    
    ##  Max.   :4.000   Max.   :11.000    Max.   :59.00                    
    ##                                                                     
    ##               action_type            shot_type    shot_distance  
    ##  Jump Shot          :2051   2PT Field Goal:2402   Min.   : 0.00  
    ##  Pullup Jump shot   : 521   3PT Field Goal:1932   1st Qu.: 2.25  
    ##  Layup Shot         : 215                         Median :19.00  
    ##  Driving Layup Shot : 160                         Mean   :15.58  
    ##  Step Back Jump shot: 133                         3rd Qu.:25.00  
    ##  Running Layup Shot : 115                         Max.   :71.00  
    ##  (Other)            :1139                                        
    ##                    opponent          x                  y        
    ##  Houston Rockets       : 271   Min.   :-248.000   Min.   :-39.0  
    ##  Sacramento Kings      : 249   1st Qu.: -56.000   1st Qu.: 13.0  
    ##  Los Angeles Clippers  : 246   Median :   1.000   Median : 95.0  
    ##  Oklahoma City Thunder : 240   Mean   :   7.746   Mean   :109.4  
    ##  Portland Trail Blazers: 238   3rd Qu.:  95.500   3rd Qu.:198.0  
    ##  Memphis Grizzlies     : 225   Max.   : 246.000   Max.   :717.0  
    ##  (Other)               :2865                                     
    ##              name          minute    
    ##  Andre Iguodala: 371   Min.   : 1.0  
    ##  Draymond Green: 578   1st Qu.:11.0  
    ##  Kevin Durant  : 915   Median :23.0  
    ##  Klay Thompson :1220   Mean   :22.8  
    ##  Stephen Curry :1250   3rd Qu.:33.0  
    ##                        Max.   :48.0  
    ## 

![](/Users/AKaffa/Desktop/summary_distance.png)

Not only are the starting lineup not scared to shoot far from the basket, they all make at least 50% of their 2pt shots. Andre Iguodala has the highest the percentage of succesful 2 pts shots although he makes 1/3 less shots and 1/3 less successful 2 pt shots than Kevin Durant who is ranks right behind Iguodala. Therefore, if frequency of shots are taken into account, Kevin Durant is certainly the king of 2 pts.

``` r
esp_2pt <- summarise(group_by(filter(esp, shot_type == "2PT Field Goal"), name), total = n(), made=length(shot_made_flag[shot_made_flag == "shot_yes"]))
esp_2pt <- mutate(esp_2pt, perc_made= made/total)
esp_2pt <- arrange(esp_2pt, desc(perc_made))
esp_2pt
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <fct>          <int> <int>     <dbl>
    ## 1 Andre Iguodala   210   134     0.638
    ## 2 Kevin Durant     643   390     0.607
    ## 3 Stephen Curry    563   304     0.540
    ## 4 Klay Thompson    640   329     0.514
    ## 5 Draymond Green   346   171     0.494

When it comes to 3pt shots, Klay Thompson leads the Warriors with a percentage shot of 0.42. While Curry attempted significantly more shots than Thompson, there was no equally big difference in the number of shots he made compared to Thompson. That explains why Curry was ranked as the top 3pt field scorer in the 2016 season followed by Klay Thompson in 2nd place- according to the statistics from the NBA Statistics website.

``` r
esp_3pt <- summarise(group_by(filter(esp, shot_type == "3PT Field Goal"), name), total = n(), made=length(shot_made_flag[shot_made_flag == "shot_yes"]))
esp_3pt <- mutate(esp_3pt, perc_made= made/total)
esp_3pt <- arrange(esp_3pt, desc(perc_made))
esp_3pt
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <fct>          <int> <int>     <dbl>
    ## 1 Klay Thompson    580   246     0.424
    ## 2 Stephen Curry    687   280     0.408
    ## 3 Kevin Durant     272   105     0.386
    ## 4 Andre Iguodala   161    58     0.360
    ## 5 Draymond Green   232    74     0.319

Overall, Kevin Durant has the highest percentage of successful shots.

``` r
esp_overall <- summarise(group_by(esp, name), total = n(), made=length(shot_made_flag[shot_made_flag == "shot_yes"]))
esp_overall <- mutate(esp_overall, perc_made= made/total)
esp_overall <- arrange(esp_overall, desc(perc_made))
esp_overall
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <fct>          <int> <int>     <dbl>
    ## 1 Kevin Durant     915   495     0.541
    ## 2 Andre Iguodala   371   192     0.518
    ## 3 Klay Thompson   1220   575     0.471
    ## 4 Stephen Curry   1250   584     0.467
    ## 5 Draymond Green   578   245     0.424

The results mentioned above are further supported by the facetted charts of the shot statistics of the players. Although Stephen Curry appears to make the most shots and also the furthest shots (both successful and unsuccessful). Kevin Durant clearly has more even distribution of successful on unsuccesful shots. Additionally, Kevin Durant's and Klay Thompson's shots are more concentrated between the 2pt and 3pt lines compared to Curry whose shots seem to be dragging towards the opponent part of the court. That is the reason why he has the furthest succesful shots among all the five players. Iguodala seems to have a safer strategy as he makes the least shots out of everyone and his percentage of succesful shot is more even. Surprisingly it appears that most shots that Draymond make are actually further from the basket than the ones he misses.

![](/Users/AKaffa/Desktop/workout01/images/gsw_shot_charts.png)

Looking at Klay Thompson's chart, which highlights which period a shot was made. We can see that compared to the rest, he makes most of the shots in the last periods. He is the most consistent shooter across periods.

Conclusions
===========

In conclusion, the five players all display different strengths in terms of shooting with slight differences in their amount of strenght compared to their teammates. That explains their great "Strength in Numbers". While Kevin Durant handles the 2pts, Klay and Curry are on the 3pts while Green manages the outside of the basket pts and Iguodala plays the safe shots.

References
==========

foxsports.com: 5 reasons the golden state warriors are the greatest team of all time. <https://www.basketball-reference.com/players/i/iguodan01.html> <http://www.nbaminer.com/shot-distances/> <https://stats.nba.com/players/shots-shotclock/?Season=2016-17&SeasonType=Regular%20Season&sort=FG2_PCT&dir=1&ShotClockRange=22-18%20Very%20Early&PerMode=Totals> <http://www.espn.com/nba/statistics/_/year/2016>
