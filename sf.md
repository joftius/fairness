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
output %>% group_by(race) %>%
  summarise_if(.predicate = function(v) is.numeric(v), .funs = funs("mean"))
```

    ## # A tibble: 6 × 12
    ##    race  year   frisked      age timestop    build eyecolor haircolr
    ##   <int> <dbl>     <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ## 1     1  2014 0.7000127 27.70532 1420.882 3.182250 1.951700 1.306269
    ## 2     2  2014 0.6861801 27.26084 1382.889 3.162282 1.953316 1.372360
    ## 3     3  2014 0.6429787 28.44883 1359.254 3.154468 2.008085 1.539681
    ## 4     4  2014 0.5628528 30.29959 1341.227 3.140384 2.316334 2.087317
    ## 5     5  2014 0.5890715 26.88005 1348.677 3.256775 1.914705 1.319858
    ## 6     6  2014 0.6304348 26.91848 1294.853 3.211957 1.961957 1.255435
    ## # ... with 4 more variables: arstmade <dbl>, predlm <dbl>, lmblind <dbl>,
    ## #   lm_lm <dbl>

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
