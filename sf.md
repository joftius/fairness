Fairness/causation: Stop and Frisk dataset
================

Data
----

From the [NYC Stop and Frisk dataset](http://www.nyclu.org/content/stop-and-frisk-data) (“NYC Stop and Frisk Data” 2014).

``` r
data <- read.csv("2014_stop_and_frisk.csv", strip.white = TRUE, header = TRUE)
# only use part of the data because of NAs, then remove NAs
data = data[,c("year","frisked","age","race","timestop","build","eyecolor","haircolr","arstmade")]
#data = data.frame(data$year,data$frisked,data$age,data$race,data$timestop,data$build,data$eyecolor,data$haircolr)
data = data[complete.cases(data),]
output <- data
t(head(data))[,1:3]
```

    ##             1    2    3
    ## year     2014 2014 2014
    ## frisked     1    1    1
    ## age        18   31   16
    ## race        1    1    1
    ## timestop 2330 1530 2100
    ## build       3    3    4
    ## eyecolor    2    2    2
    ## haircolr    1    1    1
    ## arstmade    0    1    0

Imbalances in the data
----------------------

Average outcomes by race.

``` r
data %>% group_by(race) %>% summarise(count = n(), arstmade = mean(arstmade))
```

    ## # A tibble: 6 × 3
    ##    race count   arstmade
    ##   <int> <int>      <dbl>
    ## 1     1 23561 0.14226900
    ## 2     2  2699 0.20637273
    ## 3     3  9400 0.18234043
    ## 4     4  5314 0.13172751
    ## 5     5  2251 0.09862283
    ## 6     6   184 0.08695652

Race variables:

1 - black

2 - black Hispanic

3 - white Hispanic

4 - white

5 - Asian/Pacific Islander

6 - Am. Indian/Native

Distribution plot.

``` r
ggplot(data, aes(arstmade, linetype = factor(race))) + geom_density() + theme_bw()
```

![](sf_files/figure-markdown_github/unnamed-chunk-3-1.png)

### Linear regression

Predicted averages coincide with data averages.

``` r
model.lm <- lm(arstmade ~ year+frisked+race+timestop+build+eyecolor+haircolr, data)
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
    ##    race    predlm   lmblind      lm_lm
    ##   <int>     <dbl>     <dbl>      <dbl>
    ## 1     1 0.1524285 0.1498379 0.04582093
    ## 2     2 0.1501435 0.1488467 0.04399466
    ## 3     3 0.1490408 0.1514491 0.04378405
    ## 4     4 0.1566882 0.1617486 0.04345829
    ## 5     5 0.1359320 0.1418686 0.03667406
    ## 6     6 0.1347187 0.1436578 0.03845366

Distribution plots. \# Error

### Comparing (in-sample) mean-squared prediction error

``` r
outputMSE <- data.frame((output$arstmade - output[,4:ncol(output)])^2)
outputMSE$const <- output$arstmade^2
outputMSE %>% summarise_all("mean")
```

    ##       race timestop    build eyecolor haircolr arstmade    predlm
    ## 1 5.671934  2535927 9.957129 3.792324 3.190974        0 0.1272205
    ##     lmblind     lm_lm     const
    ## 1 0.1267243 0.1383199 0.1511438

“NYC Stop and Frisk Data.” 2014. NYCLU. <http://www.nyclu.org/content/stop-and-frisk-data>.
