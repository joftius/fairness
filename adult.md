Fairness/causation: Adult dataset
================

Data
----

From the [UCI databases](https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names) Lichman (2013).

``` r
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
adult <- read.csv(url, strip.white = TRUE, header = FALSE)
names(adult) <- c("age", "workclass", "fnlwgt", "education", "educationnum",
                  "maritalstatus", "occupation", "relationship", "race", "sex",
                  "capitalgain", "capitalloss", "hoursperweek",
                  "nativecountry", "income")
adult$fnlwght <- NULL # Survey weights -- for generalization
data <- adult[,c("workclass", "occupation", "education", "educationnum",
                 "nativecountry", "race", "sex", "age")]
data$hours <- adult$hoursperweek - mean(adult$hoursperweek)
res <- "hours"
prot <- c("sex", "race")
output <- data[,c(prot, res)]
t(head(data))[,1:3]
```

    ##               1               2                  3                  
    ## workclass     "State-gov"     "Self-emp-not-inc" "Private"          
    ## occupation    "Adm-clerical"  "Exec-managerial"  "Handlers-cleaners"
    ## education     "Bachelors"     "Bachelors"        "HS-grad"          
    ## educationnum  "13"            "13"               " 9"               
    ## nativecountry "United-States" "United-States"    "United-States"    
    ## race          "White"         "White"            "White"            
    ## sex           "Male"          "Male"             "Male"             
    ## age           "39"            "50"               "38"               
    ## hours         " -0.4374559"   "-27.4374559"      " -0.4374559"

Imbalances in the data
----------------------

Average outcomes by sex/race.

``` r
data %>% group_by(sex) %>% summarise(count = n(), hours = mean(hours))
```

    ## # A tibble: 2 × 3
    ##      sex count     hours
    ##   <fctr> <int>     <dbl>
    ## 1 Female 10771 -4.027095
    ## 2   Male 21790  1.990630

``` r
data %>% group_by(race) %>% summarise(count = n(), hours = mean(hours))
```

    ## # A tibble: 5 × 3
    ##                 race count      hours
    ##               <fctr> <int>      <dbl>
    ## 1 Amer-Indian-Eskimo   311 -0.3892243
    ## 2 Asian-Pac-Islander  1039 -0.3104106
    ## 3              Black  3124 -2.0146005
    ## 4              Other   271 -0.9688212
    ## 5              White 27816  0.2516439

Distribution plot.

``` r
ggplot(data, aes(hours, linetype = sex)) + geom_density() + theme_bw()
```

![](adult_files/figure-markdown_github/unnamed-chunk-3-1.png)

### Linear regression

Predicted averages coincide with data averages.

``` r
model.lm <- lm(hours ~ ., data)
output$lm <- predict(model.lm)

model.svm <- svm(hours ~ ., data)
output$svm <- predict(model.svm)
```

### Blinded to unfair covariates

Outcomes are only slightly less biased.

``` r
lm.blind <- fairpred_blind(data, res, prot, method = "lm")
output$lmblind <- predict(lm.blind)

svm.blind <- fairpred_blind(data, res, prot, method = "svm")
output$svmblind <- predict(svm.blind)
```

### Two-stage procedure

Greedily enforces fairness first, then builds predictions.

``` r
lm.lm <- fairpred_2s(data, res, prot, method1 = "lm", method2 = "lm")
output$lm_lm <- predict(lm.lm)

lm.svm <- fairpred_2s(data, res, prot, method1 = "lm", method2 = "svm")
output$lm_svm <- predict(lm.svm)

svm.svm <- fairpred_2s(data, res, prot, method1 = "svm", method2 = "svm")
output$svm_svm <- predict(svm.svm)
```

### Penalizing unprotected coefficients

``` r
glmn <- fairpred_pen(data, res, prot)
output$glmnet <- predict(glmn$model, newx=glmn$x, s = "lambda.1se")[,1]
```

### Comparing imbalance

Subgroup means.

``` r
output %>% group_by(sex) %>%
  summarise_if(.predicate = function(v) is.numeric(v), .funs = funs("mean"))
```

    ## # A tibble: 2 × 10
    ##      sex     hours        lm        svm    lmblind   svmblind     lm_lm
    ##   <fctr>     <dbl>     <dbl>      <dbl>      <dbl>      <dbl>     <dbl>
    ## 1 Female -4.027095 -4.027095 -2.4677566 -1.8926245 -1.3309116 -1.051789
    ## 2   Male  1.990630  1.990630  0.8956853  0.9355419  0.3334879  0.519909
    ##       lm_svm    svm_svm    glmnet
    ##        <dbl>      <dbl>     <dbl>
    ## 1 -0.6505907 -0.8938784 -4.026980
    ## 2 -0.1176753  0.7332874  1.990574

``` r
output %>% group_by(race) %>%
  summarise_if(.predicate = function(v) is.numeric(v), .funs = funs("mean"))
```

    ## # A tibble: 5 × 10
    ##                 race      hours         lm        svm     lmblind
    ##               <fctr>      <dbl>      <dbl>      <dbl>       <dbl>
    ## 1 Amer-Indian-Eskimo -0.3892243 -0.3892243 -0.7839648 -0.72201882
    ## 2 Asian-Pac-Islander -0.3104106 -0.3104106 -0.4641845 -0.02226774
    ## 3              Black -2.0146005 -2.0146005 -1.4239083 -1.66195280
    ## 4              Other -0.9688212 -0.9688212 -1.0280113 -0.91267360
    ## 5              White  0.2516439  0.2516439 -0.0578891  0.20444921
    ##     svmblind      lm_lm     lm_svm    svm_svm     glmnet
    ##        <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
    ## 1 -0.5915895 -0.7462379 -0.7713912 -0.1803027 -0.4188067
    ## 2 -0.1473600  0.1353432 -0.1970486  0.2762852 -0.3084972
    ## 3 -0.9815879 -1.2287411 -0.8127852 -0.5535697 -2.0122413
    ## 4 -1.0182687 -0.7303799 -1.1332688 -0.6016333 -0.9662739
    ## 5 -0.1218372  0.1484030 -0.2257968  0.2880276  0.2516134

Distribution plots.

``` r
pd <- melt(output)
```

    ## Using sex, race as id variables

``` r
ggplot(pd, aes(value, fill = sex)) + geom_density(alpha = .3) +
  facet_wrap(~variable) + xlim(-15, 15) + theme_bw()
```

    ## Warning: Removed 6939 rows containing non-finite values (stat_density).

![](adult_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggplot(pd, aes(value, fill = race)) + geom_density(alpha = .3) +
  facet_wrap(~variable) + xlim(-15, 15) + theme_bw()
```

    ## Warning: Removed 6939 rows containing non-finite values (stat_density).

![](adult_files/figure-markdown_github/unnamed-chunk-9-2.png)

### Comparing (in-sample) mean-squared prediction error

``` r
outputMSE <- data.frame((output$hours - output[,4:ncol(output)])^2)
outputMSE$const <- output$hours^2
outputMSE %>% summarise_all("mean")
```

    ##         lm      svm  lmblind svmblind    lm_lm   lm_svm  svm_svm   glmnet
    ## 1 129.6325 121.8658 132.4799  124.644 134.1809 127.4479 124.7834 131.7256
    ##      const
    ## 1 152.4543

Lichman, M. 2013. “UCI Machine Learning Repository.” University of California, Irvine, School of Information; Computer Sciences. <http://archive.ics.uci.edu/ml>.
