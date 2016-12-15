Fairness/causation: Stop and Frisk dataset
================

Data
----

From the [UCI databases](https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names) Lichman (2013).

``` r
data <- read.csv("2014_stop_and_frisk.csv", strip.white = TRUE, header = TRUE)
output <- data
t(head(data))[,1:3]
```

    ##          1                2                       3                    
    ## year     "2014"           "2014"                  "2014"               
    ## pct      "67"             " 7"                    "84"                 
    ## ser_num  "15"             "26"                    "52"                 
    ## datestop "1012014"        "1032014"               "1042014"            
    ## timestop "2330"           "1530"                  "2100"               
    ## city     "2"              "1"                     "2"                  
    ## sex      "1"              "1"                     "1"                  
    ## race     "1"              "1"                     "1"                  
    ## dob      "12311900"       "12311900"              "12311900"           
    ## age      "18"             "31"                    "16"                 
    ## height   "67"             "67"                    "68"                 
    ## weight   "150"            "160"                   "160"                
    ## haircolr "1"              "1"                     "1"                  
    ## eyecolor "2"              "2"                     "2"                  
    ## build    " 3"             " 3"                    " 4"                 
    ## othfeatr ""               ""                      ""                   
    ## frisked  "1"              "1"                     "1"                  
    ## searched "0"              "1"                     "0"                  
    ## contrabn "0"              "0"                     "0"                  
    ## pistol   "0"              "0"                     "0"                  
    ## riflshot "0"              "0"                     "0"                  
    ## asltweap "0"              "0"                     "0"                  
    ## knifcuti "0"              "0"                     "0"                  
    ## machgun  "0"              "0"                     "0"                  
    ## othrweap "0"              "0"                     "0"                  
    ## arstmade "0"              "1"                     "0"                  
    ## arstoffn ""               "PL 170.25"             ""                   
    ## sumissue "0"              "0"                     "0"                  
    ## sumoffen ""               ""                      ""                   
    ## crimsusp "FEL"            "CRIMINAL MISCHIEF"     "GRAND LARCENY (FEL)"
    ## detailcm "20"             "23"                    "45"                 
    ## perobs   "1"              "2"                     "1"                  
    ## perstop  " 5"             " 5"                    "10"                 
    ## pf_hands "0"              "1"                     "0"                  
    ## pf_wall  "0"              "0"                     "0"                  
    ## pf_grnd  "0"              "0"                     "0"                  
    ## pf_drwep "0"              "0"                     "0"                  
    ## pf_ptwep "0"              "0"                     "0"                  
    ## pf_baton "0"              "0"                     "0"                  
    ## pf_hcuff "0"              "0"                     "0"                  
    ## pf_pepsp "0"              "0"                     "0"                  
    ## pf_other "0"              "0"                     "0"                  
    ## cs_objcs "0"              "0"                     "0"                  
    ## cs_descr "0"              "0"                     "0"                  
    ## cs_casng "0"              "1"                     "0"                  
    ## cs_lkout "0"              "0"                     "0"                  
    ## cs_cloth "0"              "0"                     "0"                  
    ## cs_drgtr "0"              "0"                     "0"                  
    ## cs_furtv "0"              "0"                     "0"                  
    ## cs_vcrim "0"              "0"                     "0"                  
    ## cs_bulge "0"              "0"                     "0"                  
    ## cs_other "1"              "0"                     "1"                  
    ## rf_vcrim "0"              "0"                     "0"                  
    ## rf_othsw "0"              "1"                     "0"                  
    ## rf_attir "0"              "0"                     "1"                  
    ## rf_vcact "0"              "0"                     "0"                  
    ## rf_rfcmp "0"              "0"                     "0"                  
    ## rf_verbl "0"              "0"                     "0"                  
    ## rf_knowl "1"              "0"                     "0"                  
    ## rf_furt  "0"              "0"                     "1"                  
    ## rf_bulg  "0"              "0"                     "0"                  
    ## sb_hdobj "0"              "0"                     "0"                  
    ## sb_outln "0"              "0"                     "0"                  
    ## sb_admis "0"              "0"                     "0"                  
    ## sb_other "0"              "1"                     "0"                  
    ## ac_proxm "0"              "0"                     "0"                  
    ## ac_evasv "0"              "0"                     "1"                  
    ## ac_assoc "0"              "0"                     "0"                  
    ## ac_cgdir "0"              "0"                     "0"                  
    ## ac_incid "0"              "0"                     "0"                  
    ## ac_time  "0"              "1"                     "0"                  
    ## ac_stsnd "0"              "0"                     "0"                  
    ## ac_rept  "0"              "0"                     "1"                  
    ## ac_inves "0"              "0"                     "0"                  
    ## ac_other "0"              "0"                     "0"                  
    ## forceuse NA               " 6"                    NA                   
    ## inout    "0"              "1"                     "1"                  
    ## trhsloc  "0"              "2"                     "0"                  
    ## premname ""               "MEZZANINE (POST 0431)" "HOTEL"              
    ## addrnum  ""               ""                      "224"                
    ## stname   ""               ""                      "DUFFIELD STREET"    
    ## stinter  "CHURCH AVENUE"  "ESSEX STREET"          "FULTON STREET"      
    ## crossst  "EAST 39 STREET" "DELANCEY STREET"       "WILLOUGHBY STREET"  
    ## addrpct  "67"             " 7"                    "84"                 
    ## sector   " 7"             " 2"                    " 6"                 
    ## beat     NA               " 2"                    NA                   
    ## post     NA               NA                      NA                   
    ## xcoord   "1000633"        " 987521"               " 988579"            
    ## ycoord   "176542"         "201066"                "191174"             
    ## typeofid "2"              "1"                     "1"                  
    ## othpers  "1"              "0"                     "1"                  
    ## explnstp "1"              "1"                     "1"                  
    ## repcmd   "186"            "863"                   " 84"                
    ## revcmd   "186"            "863"                   " 84"                
    ## offunif  "0"              "0"                     "0"                  
    ## offverb  "0"              "1"                     "1"                  
    ## officrid "0"              "0"                     "0"                  
    ## offshld  "1"              "1"                     "1"                  
    ## radio    "0"              "0"                     "0"                  
    ## recstat  "1"              "1"                     "0"                  
    ## linecm   "1"              "1"                     "1"

Imbalances in the data
----------------------

Average outcomes by race.

``` r
data %>% group_by(race) %>% summarise(count = n(), arstmade = mean(arstmade))
```

    ## # A tibble: 7 × 3
    ##    race count   arstmade
    ##   <int> <int>      <dbl>
    ## 1     1 24319 0.14174103
    ## 2     2  2789 0.20509143
    ## 3     3  9700 0.18123711
    ## 4     4  5467 0.12968721
    ## 5     5  2281 0.09864095
    ## 6     6   192 0.08854167
    ## 7    NA  1039 0.16361886

``` r
#data %>% group_by(race) %>% summarise(count = n(), arstmade = mean(arstmade))
```

Race variables: 1 - black 2 - black Hispanic 3 - white Hispanic 4 - white 5 - Asian/Pacific Islander 6 - Am. Indian/Native

Distribution plot.

``` r
ggplot(data, aes(arstmade, linetype = factor(race))) + geom_density() + theme_bw()
```

![](sf_files/figure-markdown_github/unnamed-chunk-3-1.png) Race

### Linear regression

Predicted averages coincide with data averages.

``` r
model.lm <- lm(arstmade ~ ., data)
#output$predlm <- predict(model.lm)
```

Lichman, M. 2013. “UCI Machine Learning Repository.” University of California, Irvine, School of Information; Computer Sciences. <http://archive.ics.uci.edu/ml>.
