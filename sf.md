Fairness/causation: Stop and Frisk dataset
================

Data
----

From the [NYC Stop and Frisk dataset](http://www.nyclu.org/content/stop-and-frisk-data) (“NYC Stop and Frisk Data” 2014).

``` r
data <- read.csv("2014_stop_and_frisk.csv", strip.white = TRUE, header = TRUE)
# only use part of the data because of NAs, then remove NAs
data <- data[,which(apply(data, 2, function(col) mean(is.na(col))) < .1)]
data$year <- NULL
data$linecm <- NULL
data$dob <- NULL
data$datestop <- NULL
data$repcmd <- NULL
data$revcmd <- NULL
faccols <- which(apply(data, 2, function(col) length(unique(col))) < 10)
for (col in faccols) data[,col] <- factor(data[,col])
stringcols <- which(sapply(data[1,], function(v) is.factor(v) & nlevels(v) > 10))
data <- data[,-stringcols]
data <- data[complete.cases(data),]
data$pct <- factor(data$pct)
data$race <- fct_recode(factor(data$race),
                        Black = "1", BlaskHisp = "2", Hispanic = "3",
                        White = "4", AsianPacI = "5", NativeAm = "6")
data$arstmade <- as.numeric(data$arstmade)
meanarst <- mean(data$arstmade)
data$arstmade <- data$arstmade - meanarst
output <- data[,c("race", "sex", "arstmade")]
t(head(data))[,1:3]
```

    ##          1            2            3           
    ## pct      "67"         "7"          "84"        
    ## ser_num  "15"         "26"         "52"        
    ## timestop "2330"       "1530"       "2100"      
    ## city     "2"          "1"          "2"         
    ## sex      "1"          "1"          "1"         
    ## race     "Black"      "Black"      "Black"     
    ## age      "18"         "31"         "16"        
    ## height   "67"         "67"         "68"        
    ## weight   "150"        "160"        "160"       
    ## haircolr "1"          "1"          "1"         
    ## eyecolor "2"          "2"          "2"         
    ## build    "3"          "3"          "4"         
    ## frisked  "1"          "1"          "1"         
    ## searched "0"          "1"          "0"         
    ## contrabn "0"          "0"          "0"         
    ## arstmade "-0.1515108" " 0.8484892" "-0.1515108"
    ## sumissue "0"          "0"          "0"         
    ## detailcm "20"         "23"         "45"        
    ## perobs   "1"          "2"          "1"         
    ## perstop  " 5"         " 5"         "10"        
    ## inout    "0"          "1"          "1"         
    ## trhsloc  "0"          "2"          "0"         
    ## addrpct  "67"         " 7"         "84"        
    ## sector   " 7"         " 2"         " 6"        
    ## xcoord   "1000633"    " 987521"    " 988579"   
    ## ycoord   "176542"     "201066"     "191174"    
    ## typeofid "2"          "1"          "1"         
    ## othpers  "1"          "0"          "1"         
    ## explnstp "1"          "1"          "1"         
    ## offunif  "0"          "0"          "0"         
    ## offverb  "0"          "1"          "1"         
    ## officrid "0"          "0"          "0"         
    ## offshld  "1"          "1"          "1"         
    ## radio    "0"          "0"          "0"         
    ## recstat  "1"          "1"          "0"

Imbalances in the data
----------------------

Average outcomes by race.

``` r
data %>% group_by(race) %>% summarise(count = n(), arstmade = mean(arstmade))
```

    ## # A tibble: 6 × 3
    ##        race count     arstmade
    ##      <fctr> <int>        <dbl>
    ## 1     Black 22690 -0.007967363
    ## 2 BlaskHisp  2583  0.057161313
    ## 3  Hispanic  8994  0.032611974
    ## 4     White  4952 -0.028126283
    ## 5 AsianPacI  2136 -0.050855343
    ## 6  NativeAm   180 -0.068177441

### Linear regression

Predicted averages coincide with data averages.

``` r
model.lm <- lm(arstmade ~ .-1, data)
output$predlm <- predict(model.lm)
```

### Blinded to unfair covariates

Outcomes are only slightly less biased.

``` r
lm.blind <- fairpred_blind(data, "arstmade", "race", method = "lm")
output$lmblind <- predict(lm.blind)
```

### Two-stage procedure

Greedily enforces fairness first, then builds predictions.

``` r
lm.lm <- fairpred_2s(data, "arstmade", "race", method1 = "lm", method2 = "lm")
output$lm_lm <- predict(lm.lm)
```

### Comparing imbalance

Subgroup means.

``` r
output[,c("race","predlm","lmblind","lm_lm")] %>% group_by(race) %>%
  summarise_if(.predicate = function(v) is.numeric(v), .funs = funs("mean"))
```

    ## # A tibble: 6 × 4
    ##        race       predlm     lmblind        lm_lm
    ##      <fctr>        <dbl>       <dbl>        <dbl>
    ## 1     Black -0.007967363 -0.00452223 -0.003489031
    ## 2 BlaskHisp  0.057161313  0.03666435  0.028189535
    ## 3  Hispanic  0.032611974  0.02761223  0.022224578
    ## 4     White -0.028126283 -0.02536995 -0.018604398
    ## 5 AsianPacI -0.050855343 -0.04882569 -0.042881614
    ## 6  NativeAm -0.068177441 -0.05841880 -0.054506670

Distribution plots.

``` r
pd <- melt(output)
```

    ## Using race, sex as id variables

``` r
ggplot(pd, aes(value, fill = sex)) + geom_density(alpha = .3) +
  facet_wrap(~variable) + xlim(-.5, 1) + theme_bw()
```

    ## Warning: Removed 525 rows containing non-finite values (stat_density).

![](sf_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
ggplot(pd, aes(value, fill = race)) + geom_density(alpha = .3) +
  facet_wrap(~variable) + xlim(-.5, 1) + theme_bw()
```

    ## Warning: Removed 525 rows containing non-finite values (stat_density).

![](sf_files/figure-markdown_github/unnamed-chunk-7-2.png)

### Comparing (in-sample) mean-squared prediction error

``` r
outputMSE <- data.frame((output$arstmade - output[,4:ncol(output)])^2)
outputMSE$const <- output$arstmade^2
outputMSE %>% summarise_all("mean")
```

    ##       predlm    lmblind     lm_lm     const
    ## 1 0.07490494 0.07495154 0.0750636 0.1285553

“NYC Stop and Frisk Data.” 2014. NYCLU. <http://www.nyclu.org/content/stop-and-frisk-data>.
