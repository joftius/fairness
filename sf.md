Fairness/causation: Stop and Frisk dataset
================

Data
----

From the [NYC Stop and Frisk dataset](http://www.nyclu.org/content/stop-and-frisk-data) (“NYC Stop and Frisk Data” 2014).

``` r
source("load_stopandfrisk.R")
stopandfrisk$found <- apply(stopandfrisk[,c("pistol", "knifcuti", "contrabn", "othrweapon")], 1, function(x) any(x == 1))
stopandfrisk$charged <- apply(stopandfrisk[,c("arstmade", "sumissue")], 1, function(x) any(x == 1))
stopandfrisk$force <- apply(stopandfrisk[,pf_cols], 1, function(x) any(x == 1))
data <- subset(stopandfrisk, charged == 0)
data[,pf_cols] <- NULL
data[,c("charged", "arstmade", "sumissue", "xcoord", "ycoord")] <- NULL

meanforce <- mean(data$force)
data$force <- data$force - meanforce

output <- data[,c("race", "force")]
res <- "force"
prot <- "race"
t(head(data))[,1:3]
```

    ##            1            3            5           
    ## pct        "67"         "84"         "77"        
    ## timestop   "2330"       "2100"       "1310"      
    ## city       "Brooklyn"   "Brooklyn"   "Brooklyn"  
    ## sex        "Male"       "Male"       "Male"      
    ## race       "Black"      "Black"      "Black"     
    ## age        "18"         "16"         "32"        
    ## height     "67"         "68"         "70"        
    ## weight     "150"        "160"        "200"       
    ## haircolr   "black"      "black"      "black"     
    ## eyecolor   "2"          "2"          "2"         
    ## build      "medium"     "thin"       "muscular"  
    ## frisked    "1"          "1"          "0"         
    ## searched   "0"          "0"          "0"         
    ## contrabn   "0"          "0"          "0"         
    ## pistol     "0"          "0"          "0"         
    ## knifcuti   "0"          "0"          "0"         
    ## detailcm   "20"         "45"         "46"        
    ## perobs     "1"          "1"          "1"         
    ## perstop    " 5"         "10"         " 2"        
    ## cs_objcs   "0"          "0"          "0"         
    ## cs_descr   "0"          "0"          "0"         
    ## cs_casng   "0"          "0"          "1"         
    ## cs_lkout   "0"          "0"          "0"         
    ## cs_cloth   "0"          "0"          "1"         
    ## cs_drgtr   "0"          "0"          "0"         
    ## cs_furtv   "0"          "0"          "1"         
    ## cs_vcrim   "0"          "0"          "0"         
    ## cs_bulge   "0"          "0"          "0"         
    ## cs_other   "1"          "1"          "0"         
    ## rf_vcrim   "0"          "0"          "0"         
    ## rf_othsw   "0"          "0"          "0"         
    ## rf_attir   "0"          "1"          "0"         
    ## rf_vcact   "0"          "0"          "0"         
    ## rf_rfcmp   "0"          "0"          "0"         
    ## rf_verbl   "0"          "0"          "0"         
    ## rf_knowl   "1"          "0"          "0"         
    ## rf_furt    "0"          "1"          "0"         
    ## rf_bulg    "0"          "0"          "0"         
    ## sb_hdobj   "0"          "0"          "0"         
    ## sb_outln   "0"          "0"          "0"         
    ## sb_admis   "0"          "0"          "0"         
    ## sb_other   "0"          "0"          "0"         
    ## ac_proxm   "0"          "0"          "0"         
    ## ac_evasv   "0"          "1"          "0"         
    ## ac_assoc   "0"          "0"          "0"         
    ## ac_cgdir   "0"          "0"          "1"         
    ## ac_incid   "0"          "0"          "1"         
    ## ac_time    "0"          "0"          "1"         
    ## ac_stsnd   "0"          "0"          "0"         
    ## ac_rept    "0"          "1"          "0"         
    ## ac_inves   "0"          "0"          "0"         
    ## ac_other   "0"          "0"          "0"         
    ## forceuse   "0"          "0"          "0"         
    ## inout      "0"          "1"          "1"         
    ## trhsloc    "0"          "0"          "0"         
    ## sector     "7"          "6"          "10"        
    ## othpers    "1"          "1"          "0"         
    ## repcmd     "186"        " 84"        "187"       
    ## offunif    "0"          "0"          "0"         
    ## radio      "0"          "0"          "0"         
    ## othrweapon "FALSE"      "FALSE"      "FALSE"     
    ## perobs5    "FALSE"      "FALSE"      "FALSE"     
    ## perstop10  "FALSE"      " TRUE"      "FALSE"     
    ## found      "FALSE"      "FALSE"      "FALSE"     
    ## force      "-0.2107577" "-0.2107577" "-0.2107577"

``` r
#print(reasoncols)
```

Imbalances in the data
----------------------

Average outcomes by race.

``` r
data %>% group_by(race) %>% summarise(count = n(), force = mean(force) + meanforce)
```

    ## # A tibble: 6 × 3
    ##        race count     force
    ##      <fctr> <int>     <dbl>
    ## 1     Black 18715 0.2117553
    ## 2 BlaskHisp  1948 0.2931211
    ## 3  Hispanic  7030 0.2403983
    ## 4     White  4160 0.1509615
    ## 5 AsianPacI  1876 0.1428571
    ## 6  NativeAm   163 0.1411043

### Linear regression

Predicted averages coincide with data averages.

``` r
model.lm <- lm(force ~ .-1, data)
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
output <- output %>% mutate_if(is.numeric, funs(. + meanforce))
output %>% group_by(race) %>% summarise_if(is.numeric, .funs = funs("mean"))
```

    ## # A tibble: 6 × 7
    ##        race     force    predlm   lmblind     lm_lm     ridge      enet
    ##      <fctr>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1     Black 0.2117553 0.2117553 0.2109901 0.2082616 0.2108104 0.2107411
    ## 2 BlaskHisp 0.2931211 0.2931211 0.2869178 0.2758235 0.2861306 0.2860867
    ## 3  Hispanic 0.2403983 0.2403983 0.2416532 0.2379446 0.2409163 0.2416224
    ## 4     White 0.1509615 0.1509615 0.1528279 0.1713836 0.1546058 0.1547605
    ## 5 AsianPacI 0.1428571 0.1428571 0.1500399 0.1609695 0.1522324 0.1502067
    ## 6  NativeAm 0.1411043 0.1411043 0.1186678 0.1251216 0.1229983 0.1183659

Distribution plots.

``` r
pd <- melt(output)
```

    ## Using race as id variables

``` r
ggplot(pd, aes(value, fill = race)) + geom_density(alpha = .3) +
  facet_wrap(~variable) + xlim(-.5, 1) + theme_bw()
```

    ## Warning: Removed 9374 rows containing non-finite values (stat_density).

![](sf_files/figure-markdown_github/unnamed-chunk-8-1.png)

### Comparing (in-sample) mean-squared prediction error

``` r
outputMSE <- data.frame((output$force - output[,3:ncol(output)])^2)
outputMSE$const <- output$force^2
outputMSE %>% summarise_all("mean")
```

    ##      predlm    lmblind      lm_lm      ridge     enet     const
    ## 1 0.0307583 0.03076829 0.03102454 0.03135395 0.030806 0.2107577

“NYC Stop and Frisk Data.” 2014. NYCLU. <http://www.nyclu.org/content/stop-and-frisk-data>.
