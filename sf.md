Fairness/causation: Stop and Frisk dataset
================

Data
----

From the [NYC Stop and Frisk dataset](http://www.nyclu.org/content/stop-and-frisk-data) (“NYC Stop and Frisk Data” 2014).

``` r
source("load_stopandfrisk.R")
data <- stopandfrisk
data$arstmade <- as.numeric(data$arstmade) -1
meanarst <- mean(data$arstmade)
data$arstmade <- data$arstmade - meanarst
output <- data[,c("race", "sex", "arstmade")]
res <- "arstmade"
prot <- c("race", "sex")
t(head(data))[,1:3]
```

    ##            1            2            3           
    ## pct        "67"         "7"          "84"        
    ## datestop   "1012014"    "1032014"    "1042014"   
    ## timestop   "2330"       "1530"       "2100"      
    ## city       "Brooklyn"   "Manhattan"  "Brooklyn"  
    ## sex        "Male"       "Male"       "Male"      
    ## race       "Black"      "Black"      "Black"     
    ## age        "18"         "31"         "16"        
    ## height     "67"         "67"         "68"        
    ## weight     "150"        "160"        "160"       
    ## haircolr   "black"      "black"      "black"     
    ## eyecolor   "2"          "2"          "2"         
    ## build      "medium"     "medium"     "thin"      
    ## frisked    "1"          "1"          "1"         
    ## searched   "0"          "1"          "0"         
    ## contrabn   "0"          "0"          "0"         
    ## pistol     "0"          "0"          "0"         
    ## knifcuti   "0"          "0"          "0"         
    ## arstmade   "-0.1519095" " 0.8480905" "-0.1519095"
    ## sumissue   "0"          "0"          "0"         
    ## detailcm   "20"         "23"         "45"        
    ## perobs     "1"          "2"          "1"         
    ## perstop    " 5"         " 5"         "10"        
    ## pf_hands   "0"          "1"          "0"         
    ## pf_wall    "0"          "0"          "0"         
    ## pf_grnd    "0"          "0"          "0"         
    ## pf_drwep   "0"          "0"          "0"         
    ## pf_ptwep   "0"          "0"          "0"         
    ## pf_hcuff   "0"          "0"          "0"         
    ## cs_objcs   "0"          "0"          "0"         
    ## cs_descr   "0"          "0"          "0"         
    ## cs_casng   "0"          "1"          "0"         
    ## cs_lkout   "0"          "0"          "0"         
    ## cs_cloth   "0"          "0"          "0"         
    ## cs_drgtr   "0"          "0"          "0"         
    ## cs_furtv   "0"          "0"          "0"         
    ## cs_vcrim   "0"          "0"          "0"         
    ## cs_bulge   "0"          "0"          "0"         
    ## cs_other   "1"          "0"          "1"         
    ## rf_vcrim   "0"          "0"          "0"         
    ## rf_othsw   "0"          "1"          "0"         
    ## rf_attir   "0"          "0"          "1"         
    ## rf_vcact   "0"          "0"          "0"         
    ## rf_rfcmp   "0"          "0"          "0"         
    ## rf_verbl   "0"          "0"          "0"         
    ## rf_knowl   "1"          "0"          "0"         
    ## rf_furt    "0"          "0"          "1"         
    ## rf_bulg    "0"          "0"          "0"         
    ## sb_hdobj   "0"          "0"          "0"         
    ## sb_outln   "0"          "0"          "0"         
    ## sb_admis   "0"          "0"          "0"         
    ## sb_other   "0"          "1"          "0"         
    ## ac_proxm   "0"          "0"          "0"         
    ## ac_evasv   "0"          "0"          "1"         
    ## ac_assoc   "0"          "0"          "0"         
    ## ac_cgdir   "0"          "0"          "0"         
    ## ac_incid   "0"          "0"          "0"         
    ## ac_time    "0"          "1"          "0"         
    ## ac_stsnd   "0"          "0"          "0"         
    ## ac_rept    "0"          "0"          "1"         
    ## ac_inves   "0"          "0"          "0"         
    ## ac_other   "0"          "0"          "0"         
    ## inout      "0"          "1"          "1"         
    ## trhsloc    "0"          "2"          "0"         
    ## sector     "7"          "2"          "6"         
    ## xcoord     "1000633"    " 987521"    " 988579"   
    ## ycoord     "176542"     "201066"     "191174"    
    ## othpers    "1"          "0"          "1"         
    ## explnstp   "1"          "1"          "1"         
    ## offunif    "0"          "0"          "0"         
    ## radio      "0"          "0"          "0"         
    ## othrweapon "FALSE"      "FALSE"      "FALSE"     
    ## pf_othr    "FALSE"      "FALSE"      "FALSE"     
    ## perobs5    "FALSE"      "FALSE"      "FALSE"     
    ## perstop10  "FALSE"      "FALSE"      " TRUE"

``` r
print(reasoncols)
```

    ##  [1] "pf_hands" "pf_wall"  "pf_grnd"  "pf_drwep" "pf_ptwep" "pf_hcuff"
    ##  [7] "cs_objcs" "cs_descr" "cs_casng" "cs_lkout" "cs_cloth" "cs_drgtr"
    ## [13] "cs_furtv" "cs_vcrim" "cs_bulge" "cs_other" "rf_vcrim" "rf_othsw"
    ## [19] "rf_attir" "rf_vcact" "rf_rfcmp" "rf_verbl" "rf_knowl" "rf_furt" 
    ## [25] "rf_bulg"  "sb_hdobj" "sb_outln" "sb_admis" "sb_other" "ac_proxm"
    ## [31] "ac_evasv" "ac_assoc" "ac_cgdir" "ac_incid" "ac_time"  "ac_stsnd"
    ## [37] "ac_rept"  "ac_inves" "ac_other" "pf_othr"

Imbalances in the data
----------------------

Average outcomes by race.

``` r
data %>% group_by(race) %>% summarise(count = n(), arstmade = meanarst + mean(arstmade))
```

    ## # A tibble: 6 × 3
    ##        race count   arstmade
    ##      <fctr> <int>      <dbl>
    ## 1     Black 22495 0.14398755
    ## 2 BlaskHisp  2565 0.20857700
    ## 3  Hispanic  8928 0.18458781
    ## 4     White  4900 0.12367347
    ## 5 AsianPacI  2122 0.10084826
    ## 6  NativeAm   179 0.08379888

### Linear regression

Predicted averages coincide with data averages.

``` r
model.lm <- lm(arstmade ~ .-1, data)
output$predlm <- predict(model.lm)
```

### Blinded to unfair covariates

Outcomes are only slightly less biased.

``` r
lm.blind <- fairpred_blind(data, res, prot, method = "lm")
output$lmblind <- predict(lm.blind)
```

### Two-stage procedure

Greedily enforces fairness first, then builds predictions.

``` r
lm.lm <- fairpred_2s(data, res, prot, method1 = "lm", method2 = "lm")
output$lm_lm <- predict(lm.lm)
```

### Penalizing unprotected coefficients

``` r
ridge <- fairpred_pen(data, res, prot, alpha = 0)
output$ridge <- ridge$predictions

enet <- fairpred_pen(data, res, prot, alpha = .5)
output$enet <- enet$predictions
```

### Comparing imbalance

Subgroup means.

``` r
output <- output %>% mutate_if(is.numeric, funs(. + meanarst))
output %>% group_by(race) %>% summarise_if(is.numeric, .funs = funs("mean"))
```

    ## # A tibble: 6 × 7
    ##        race   arstmade     predlm    lmblind     lm_lm     ridge
    ##      <fctr>      <dbl>      <dbl>      <dbl>     <dbl>     <dbl>
    ## 1     Black 0.14398755 0.14398755 0.14728842 0.1490948 0.1490071
    ## 2 BlaskHisp 0.20857700 0.20857700 0.18698521 0.1779070 0.1844254
    ## 3  Hispanic 0.18458781 0.18458781 0.18091879 0.1739136 0.1780497
    ## 4     White 0.12367347 0.12367347 0.12586077 0.1325097 0.1249142
    ## 5 AsianPacI 0.10084826 0.10084826 0.10110988 0.1067308 0.1001353
    ## 6  NativeAm 0.08379888 0.08379888 0.09840219 0.1022352 0.0985945
    ##         enet
    ##        <dbl>
    ## 1 0.14950524
    ## 2 0.18448862
    ## 3 0.17771876
    ## 4 0.12317308
    ## 5 0.09933838
    ## 6 0.09767149

Distribution plots.

``` r
pd <- melt(output)
```

    ## Using race, sex as id variables

``` r
ggplot(pd, aes(value, fill = sex)) + geom_density(alpha = .3) +
  facet_wrap(~variable) + xlim(-.5, 1) + theme_bw()
```

    ## Warning: Removed 3818 rows containing non-finite values (stat_density).

![](sf_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggplot(pd, aes(value, fill = race)) + geom_density(alpha = .3) +
  facet_wrap(~variable) + xlim(-.5, 1) + theme_bw()
```

    ## Warning: Removed 3818 rows containing non-finite values (stat_density).

![](sf_files/figure-markdown_github/unnamed-chunk-8-2.png)

### Comparing (in-sample) mean-squared prediction error

``` r
outputMSE <- data.frame((output$arstmade - output[,4:ncol(output)])^2)
outputMSE$const <- output$arstmade^2
outputMSE %>% summarise_all("mean")
```

    ##       predlm   lmblind      lm_lm     ridge       enet     const
    ## 1 0.06420802 0.0642947 0.06449954 0.0644255 0.06437897 0.1519095

“NYC Stop and Frisk Data.” 2014. NYCLU. <http://www.nyclu.org/content/stop-and-frisk-data>.
