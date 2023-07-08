Online News Popularity Analysis
================
Rina Deka and Jessica Ayers
2023-06-29

# Introduction

**RINA**

*You should have an introduction section that briefly describes the data
and the variables you have to work with (just discuss the ones you want
to use). Your target variables is the shares variable.*

*You should also mention the purpose of your analysis and the methods
you’ll use to model the response. You’ll describe those in more detail
later.*

# Data

**RINA or JESS**

*Use a relative path to import the data. Subset the data to work on the
data channel of interest.*

``` r
library(tidyverse)
#import data set
#my data set is stored in the folder above where I have the project 2 folder
onpdata <- read_csv("../OnlineNewsPopularity/OnlineNewsPopularity.csv")
#find names of variables
attributes(onpdata)$names
```

    ##  [1] "url"                           "timedelta"                    
    ##  [3] "n_tokens_title"                "n_tokens_content"             
    ##  [5] "n_unique_tokens"               "n_non_stop_words"             
    ##  [7] "n_non_stop_unique_tokens"      "num_hrefs"                    
    ##  [9] "num_self_hrefs"                "num_imgs"                     
    ## [11] "num_videos"                    "average_token_length"         
    ## [13] "num_keywords"                  "data_channel_is_lifestyle"    
    ## [15] "data_channel_is_entertainment" "data_channel_is_bus"          
    ## [17] "data_channel_is_socmed"        "data_channel_is_tech"         
    ## [19] "data_channel_is_world"         "kw_min_min"                   
    ## [21] "kw_max_min"                    "kw_avg_min"                   
    ## [23] "kw_min_max"                    "kw_max_max"                   
    ## [25] "kw_avg_max"                    "kw_min_avg"                   
    ## [27] "kw_max_avg"                    "kw_avg_avg"                   
    ## [29] "self_reference_min_shares"     "self_reference_max_shares"    
    ## [31] "self_reference_avg_sharess"    "weekday_is_monday"            
    ## [33] "weekday_is_tuesday"            "weekday_is_wednesday"         
    ## [35] "weekday_is_thursday"           "weekday_is_friday"            
    ## [37] "weekday_is_saturday"           "weekday_is_sunday"            
    ## [39] "is_weekend"                    "LDA_00"                       
    ## [41] "LDA_01"                        "LDA_02"                       
    ## [43] "LDA_03"                        "LDA_04"                       
    ## [45] "global_subjectivity"           "global_sentiment_polarity"    
    ## [47] "global_rate_positive_words"    "global_rate_negative_words"   
    ## [49] "rate_positive_words"           "rate_negative_words"          
    ## [51] "avg_positive_polarity"         "min_positive_polarity"        
    ## [53] "max_positive_polarity"         "avg_negative_polarity"        
    ## [55] "min_negative_polarity"         "max_negative_polarity"        
    ## [57] "title_subjectivity"            "title_sentiment_polarity"     
    ## [59] "abs_title_subjectivity"        "abs_title_sentiment_polarity" 
    ## [61] "shares"

``` r
#want to subset to one category of data_channel_is*
#using data_channel_is_world for the primary analysis
onpdata_world <- subset(onpdata, data_channel_is_world == 1)
# onpdata_lifestyle <- subset(onpdata, data_channel_is_lifestyle == 1)
# onpdata_entertainment <- subset(onpdata, data_channel_is_entertainment == 1)
# onpdata_bus <- subset(onpdata, data_channel_is_bus == 1)
# onpdata_socmed <- subset(onpdata, data_channel_is_socmed == 1)
# onpdata_tech <- subset(onpdata, data_channel_is_tech == 1)

#check that subset is correct
head(onpdata_world$data_channel_is_world)
```

    ## [1] 1 1 1 1 1 1

# Summarizations

\*\*RINA and JESS

``` r
#create training and test data set
library(caret)
set.seed(111)
trainIndex <- createDataPartition(onpdata_world$shares, p = 0.7, list = FALSE) 
worldTrain <- onpdata_world[trainIndex, ]
worldTest <- onpdata_world[-trainIndex, ]
```

*You should produce some basic (but meaningful) summary statistics and
plots about the training data you are working with (especially as it
relates to your response).*

``` r
summary(worldTrain$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41     827    1100    2272    1900  128500

The above summary provides the minimum, maximum, median, and mean of the
number of shares in the training data. The first and third quantile are
also included.

Since our response variable is the number of shares, we can first look
at when the articles were published and the frequency for each day.

``` r
m <- sum(worldTrain$weekday_is_monday)
tu <- sum(worldTrain$weekday_is_tuesday)
wed <- sum(worldTrain$weekday_is_wednesday)
th <- sum(worldTrain$weekday_is_thursday)
f <- sum(worldTrain$weekday_is_friday)
sat <- sum(worldTrain$weekday_is_saturday)
sun <-sum(worldTrain$weekday_is_sunday)
data.frame(monday = m, tuesday = tu, wednesday = wed, thursday = th, friday = f, saturday = sat, sunday = sun)
```

    ##   monday tuesday wednesday thursday friday saturday sunday
    ## 1    947    1087      1092     1083    915      369    407

``` r
days <- c(m, tu, wed, th, f, sat, sun)
```

From the above sums we can identify which day has the largest amount of
published articles. We can visualize this by creating a bar graph as
seen below.

``` r
plot <- barplot(days, main = "Frequency of published articles on each day", ylab = "Count", xlab = "Day",names.arg = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"), col = "blue")
```

![](./images/unnamed-chunk-22-1.png)<!-- -->

We can also look at different attributes that the articles have such as
number of images and number of number of videos. We can explore if there
are more shares with more images or videos.

``` r
worldTrain %>%
  group_by(num_imgs, num_videos) %>%
  summarise(mean = mean(shares), sd = sd(shares))
```

    ## `summarise()` has grouped output by 'num_imgs'. You can override using the `.groups` argument.

    ## # A tibble: 169 × 4
    ## # Groups:   num_imgs [45]
    ##    num_imgs num_videos   mean     sd
    ##       <dbl>      <dbl>  <dbl>  <dbl>
    ##  1        0          0  2377.  3254.
    ##  2        0          1  3017.  6217.
    ##  3        0          2  4857. 14080.
    ##  4        0          3  7825. 13956.
    ##  5        0          4 12100     NA 
    ##  6        0          5  5900     NA 
    ##  7        0          6  1500     NA 
    ##  8        0          8  1100     NA 
    ##  9        0          9  1967.   776.
    ## 10        0         10  2400   1556.
    ## # ℹ 159 more rows

Let’s visualize this. First for number of images:

``` r
ggplot(worldTrain, aes(x = num_imgs, y = shares)) +
  geom_point() +
    geom_smooth(method = "lm", col = "green") + 
  geom_smooth() + 
   labs(title = "Number of Shares vs Number of Images", x = "Number of Images", y = "Number of Shares")
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](./images/unnamed-chunk-24-1.png)<!-- -->

From the above plot if the trend shows a positive linear line, as number
of images included in the article increases as does the number of
shares. If it shows a negative linear line, the number of shares
decreases with the addition of images. If no trend is shown, the number
of images included has no impact on overall shares.

For number of videos:

``` r
ggplot(worldTrain, aes(x = num_videos, y = shares)) +
  geom_point() + 
  geom_smooth(method = "lm", col = "green") + 
  geom_smooth() + 
  labs(title = "Number of Shares vs Number of Videos", x = "Number of Videos", y = "Number of Shares")
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](./images/unnamed-chunk-25-1.png)<!-- -->

Similar to above, from the above plot if the trend shows a positive
linear line, as number of videos included in the article increases as
does the number of shares. If it shows a negative linear line, the
number of shares decreases with the addition of videos If no trend is
shown, the number of videos included has no impact on overall shares.

``` r
Correlation <- cor(select(worldTrain, 2:13), method = "spearman")
#install corrplot library
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
corrplot(Correlation, type = "upper", tl.pos = "lt")
```

![](./images/unnamed-chunk-26-1.png)<!-- -->

``` r
Correlation2 <-cor(select(worldTrain, 20:40))
corrplot(Correlation2, type = "upper", tl.pos = "lt")
```

![](./images/unnamed-chunk-26-2.png)<!-- -->

``` r
Correlation3 <-cor(select(worldTrain, 41:60))
corrplot(Correlation3, type = "upper", tl.pos = "lt")
```

![](./images/unnamed-chunk-26-3.png)<!-- -->

*As you will automate this same analysis across other data, you can’t
describe the trends you see in the graph (unless you want to try to
automate that!). You should describe what to look for in the summary
statistics/plots to help the reader understand the summary or graph.*

# Modeling

**RINA: linear regression & boosted tree model & explanation of the
ensemble model you are using**

**JESS: linear regression & random forest model & explanation of the
idea of a linear regression model & explanation of the ensemble model
you are using**

``` r
worldTest <- worldTest %>%
  select(-url, -data_channel_is_bus, -data_channel_is_lifestyle, -data_channel_is_entertainment, - data_channel_is_socmed, - data_channel_is_tech, - data_channel_is_world)

worldTrain <- worldTrain %>%
  select(-url, -data_channel_is_bus, -data_channel_is_lifestyle, -data_channel_is_entertainment, - data_channel_is_socmed, - data_channel_is_tech, - data_channel_is_world)

lmFit2 <- train(shares ~ .^2, data = worldTrain, method = "lm",
trControl = trainControl(method = "cv", number = 5))

summary(lmFit2)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -14799  -1756   -284   1109  74824 
    ## 
    ## Coefficients: (237 not defined because of singularities)
    ##                                                             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                                               -8.949e+02  1.980e+04  -0.045 0.963946
    ## timedelta                                                  2.747e+01  2.215e+01   1.240 0.214891
    ## n_tokens_title                                            -3.137e+02  6.266e+02  -0.501 0.616719
    ## n_tokens_content                                           2.740e+00  1.263e+01   0.217 0.828293
    ## n_unique_tokens                                            1.793e+04  7.650e+04   0.234 0.814669
    ## n_non_stop_words                                          -1.364e+04  4.834e+04  -0.282 0.777824
    ## n_non_stop_unique_tokens                                  -2.840e+04  6.440e+04  -0.441 0.659247
    ## num_hrefs                                                 -1.911e+02  3.934e+02  -0.486 0.627220
    ## num_self_hrefs                                            -2.056e+03  1.235e+03  -1.665 0.095969
    ## num_imgs                                                   2.883e+02  3.447e+02   0.836 0.402976
    ## num_videos                                                 6.481e+02  1.427e+03   0.454 0.649641
    ## average_token_length                                      -8.288e+02  8.074e+03  -0.103 0.918251
    ## num_keywords                                               1.132e+03  8.079e+02   1.401 0.161211
    ## kw_min_min                                                 1.281e+02  8.749e+01   1.465 0.143071
    ## kw_max_min                                                -1.527e+00  2.604e+00  -0.586 0.557624
    ## kw_avg_min                                                 5.473e+00  1.549e+01   0.353 0.723930
    ## kw_min_max                                                -8.357e-02  1.888e-01  -0.443 0.658055
    ## kw_max_max                                                 6.885e-03  2.122e-02   0.325 0.745546
    ## kw_avg_max                                                -1.485e-03  2.907e-02  -0.051 0.959265
    ## kw_min_avg                                                 2.406e+00  2.444e+00   0.984 0.324926
    ## kw_max_avg                                                 9.728e-01  1.066e+00   0.913 0.361483
    ## kw_avg_avg                                                -5.520e+00  4.277e+00  -1.291 0.196880
    ## self_reference_min_shares                                 -2.782e+00  2.754e+00  -1.010 0.312466
    ## self_reference_max_shares                                 -8.810e-01  1.785e+00  -0.494 0.621595
    ## self_reference_avg_sharess                                 2.902e+00  4.409e+00   0.658 0.510376
    ## weekday_is_monday                                         -1.163e+04  5.933e+03  -1.961 0.049988
    ## weekday_is_tuesday                                        -9.308e+03  5.876e+03  -1.584 0.113241
    ## weekday_is_wednesday                                      -1.141e+04  6.001e+03  -1.901 0.057346
    ## weekday_is_thursday                                       -6.926e+03  5.953e+03  -1.163 0.244753
    ## weekday_is_friday                                         -8.171e+03  6.054e+03  -1.350 0.177163
    ## weekday_is_saturday                                        4.366e+03  7.609e+03   0.574 0.566173
    ## weekday_is_sunday                                                 NA         NA      NA       NA
    ## is_weekend                                                        NA         NA      NA       NA
    ## LDA_00                                                    -6.531e+02  1.458e+04  -0.045 0.964285
    ## LDA_01                                                     2.004e+03  1.644e+04   0.122 0.902975
    ## LDA_02                                                     7.553e+03  8.379e+03   0.901 0.367448
    ## LDA_03                                                    -9.218e+03  1.287e+04  -0.716 0.473791
    ## LDA_04                                                            NA         NA      NA       NA
    ## global_subjectivity                                       -3.769e+04  3.440e+04  -1.096 0.273334
    ## global_sentiment_polarity                                  6.466e+04  8.239e+04   0.785 0.432649
    ## global_rate_positive_words                                 7.225e+05  4.376e+05   1.651 0.098779
    ## global_rate_negative_words                                 3.401e+05  6.218e+05   0.547 0.584369
    ## rate_positive_words                                       -3.093e+03  4.595e+04  -0.067 0.946332
    ## rate_negative_words                                               NA         NA      NA       NA
    ## avg_positive_polarity                                      3.721e+04  5.850e+04   0.636 0.524783
    ## min_positive_polarity                                     -1.430e+03  5.022e+04  -0.028 0.977291
    ## max_positive_polarity                                      5.977e+03  1.807e+04   0.331 0.740840
    ## avg_negative_polarity                                     -2.489e+04  5.432e+04  -0.458 0.646772
    ## min_negative_polarity                                      1.198e+04  1.893e+04   0.633 0.526742
    ## max_negative_polarity                                      3.421e+04  5.021e+04   0.681 0.495751
    ## title_subjectivity                                         2.352e+04  6.781e+03   3.468 0.000529
    ## title_sentiment_polarity                                   2.249e+03  6.138e+03   0.366 0.714137
    ## abs_title_subjectivity                                     1.604e+04  8.455e+03   1.898 0.057804
    ## abs_title_sentiment_polarity                              -1.113e+04  1.042e+04  -1.068 0.285666
    ## `timedelta:n_tokens_title`                                -2.668e-02  2.335e-01  -0.114 0.909024
    ## `timedelta:n_tokens_content`                              -3.192e-03  2.395e-03  -1.333 0.182589
    ## `timedelta:n_unique_tokens`                               -6.167e+00  1.907e+01  -0.323 0.746391
    ## `timedelta:n_non_stop_words`                               2.007e+01  1.534e+01   1.308 0.190856
    ## `timedelta:n_non_stop_unique_tokens`                       8.533e+00  1.573e+01   0.543 0.587486
    ## `timedelta:num_hrefs`                                      1.044e-01  7.095e-02   1.471 0.141306
    ## `timedelta:num_self_hrefs`                                -6.619e-01  2.196e-01  -3.014 0.002592
    ## `timedelta:num_imgs`                                       4.387e-01  1.446e-01   3.034 0.002426
    ## `timedelta:num_videos`                                     8.192e-01  5.312e-01   1.542 0.123133
    ## `timedelta:average_token_length`                          -1.423e+00  2.277e+00  -0.625 0.531976
    ## `timedelta:num_keywords`                                  -8.530e-01  3.379e-01  -2.524 0.011624
    ## `timedelta:kw_min_min`                                    -1.960e-01  1.135e-01  -1.727 0.084228
    ## `timedelta:kw_max_min`                                    -1.941e-05  1.067e-03  -0.018 0.985487
    ## `timedelta:kw_avg_min`                                    -4.058e-03  5.975e-03  -0.679 0.497056
    ## `timedelta:kw_min_max`                                     2.343e-05  4.816e-05   0.486 0.626687
    ## `timedelta:kw_max_max`                                    -3.105e-05  2.479e-05  -1.252 0.210492
    ## `timedelta:kw_avg_max`                                    -3.733e-06  9.762e-06  -0.382 0.702147
    ## `timedelta:kw_min_avg`                                    -8.310e-04  9.036e-04  -0.920 0.357812
    ## `timedelta:kw_max_avg`                                    -3.001e-04  4.203e-04  -0.714 0.475237
    ## `timedelta:kw_avg_avg`                                     1.452e-03  1.757e-03   0.826 0.408675
    ## `timedelta:self_reference_min_shares`                      8.341e-04  4.404e-04   1.894 0.058289
    ## `timedelta:self_reference_max_shares`                      7.718e-04  2.870e-04   2.690 0.007179
    ## `timedelta:self_reference_avg_sharess`                    -1.322e-03  6.882e-04  -1.921 0.054835
    ## `timedelta:weekday_is_monday`                              1.543e+00  2.272e+00   0.679 0.496982
    ## `timedelta:weekday_is_tuesday`                             5.702e-01  2.237e+00   0.255 0.798796
    ## `timedelta:weekday_is_wednesday`                           3.271e-01  2.230e+00   0.147 0.883359
    ## `timedelta:weekday_is_thursday`                            1.602e+00  2.224e+00   0.720 0.471465
    ## `timedelta:weekday_is_friday`                              3.075e-01  2.283e+00   0.135 0.892868
    ## `timedelta:weekday_is_saturday`                           -4.139e-01  2.741e+00  -0.151 0.879989
    ## `timedelta:weekday_is_sunday`                                     NA         NA      NA       NA
    ## `timedelta:is_weekend`                                            NA         NA      NA       NA
    ## `timedelta:LDA_00`                                         7.777e+00  5.743e+00   1.354 0.175729
    ## `timedelta:LDA_01`                                        -2.266e+00  7.305e+00  -0.310 0.756433
    ## `timedelta:LDA_02`                                        -2.655e+00  3.374e+00  -0.787 0.431349
    ## `timedelta:LDA_03`                                         4.448e+00  5.852e+00   0.760 0.447223
    ## `timedelta:LDA_04`                                                NA         NA      NA       NA
    ## `timedelta:global_subjectivity`                            7.324e-01  7.658e+00   0.096 0.923808
    ## `timedelta:global_sentiment_polarity`                     -1.725e+01  1.790e+01  -0.964 0.335146
    ## `timedelta:global_rate_positive_words`                     1.279e+02  8.151e+01   1.569 0.116684
    ## `timedelta:global_rate_negative_words`                    -2.642e+02  1.439e+02  -1.835 0.066514
    ## `timedelta:rate_positive_words`                           -1.120e+01  1.044e+01  -1.072 0.283694
    ## `timedelta:rate_negative_words`                                   NA         NA      NA       NA
    ## `timedelta:avg_positive_polarity`                          1.863e+01  1.196e+01   1.557 0.119498
    ## `timedelta:min_positive_polarity`                         -1.945e+01  1.084e+01  -1.794 0.072844
    ## `timedelta:max_positive_polarity`                         -1.256e+00  3.473e+00  -0.362 0.717597
    ## `timedelta:avg_negative_polarity`                          3.117e+00  1.001e+01   0.311 0.755527
    ## `timedelta:min_negative_polarity`                          2.035e+00  3.481e+00   0.585 0.558893
    ## `timedelta:max_negative_polarity`                          4.934e+00  1.031e+01   0.479 0.632186
    ## `timedelta:title_subjectivity`                            -2.549e+00  2.431e+00  -1.049 0.294438
    ## `timedelta:title_sentiment_polarity`                       4.601e-01  2.368e+00   0.194 0.845958
    ## `timedelta:abs_title_subjectivity`                        -2.901e+00  3.209e+00  -0.904 0.366011
    ## `timedelta:abs_title_sentiment_polarity`                  -4.769e-01  3.702e+00  -0.129 0.897517
    ## `n_tokens_title:n_tokens_content`                          4.977e-02  1.656e-01   0.301 0.763733
    ## `n_tokens_title:n_unique_tokens`                           1.011e+03  1.353e+03   0.747 0.454965
    ## `n_tokens_title:n_non_stop_words`                          1.946e+03  1.064e+03   1.829 0.067526
    ## `n_tokens_title:n_non_stop_unique_tokens`                 -1.148e+03  1.127e+03  -1.019 0.308311
    ## `n_tokens_title:num_hrefs`                                -6.251e+00  5.260e+00  -1.188 0.234707
    ## `n_tokens_title:num_self_hrefs`                           -2.915e+01  1.650e+01  -1.766 0.077421
    ## `n_tokens_title:num_imgs`                                 -2.365e+01  9.180e+00  -2.577 0.010008
    ## `n_tokens_title:num_videos`                               -8.062e+00  3.277e+01  -0.246 0.805702
    ## `n_tokens_title:average_token_length`                     -2.126e+02  1.608e+02  -1.322 0.186376
    ## `n_tokens_title:num_keywords`                              1.654e+01  2.252e+01   0.735 0.462640
    ## `n_tokens_title:kw_min_min`                                1.567e-01  1.775e+00   0.088 0.929658
    ## `n_tokens_title:kw_max_min`                               -1.999e-01  8.430e-02  -2.371 0.017779
    ## `n_tokens_title:kw_avg_min`                                1.166e+00  4.699e-01   2.482 0.013089
    ## `n_tokens_title:kw_min_max`                               -2.609e-03  2.382e-03  -1.095 0.273577
    ## `n_tokens_title:kw_max_max`                                1.187e-04  5.868e-04   0.202 0.839696
    ## `n_tokens_title:kw_avg_max`                                8.675e-04  6.581e-04   1.318 0.187458
    ## `n_tokens_title:kw_min_avg`                               -1.511e-02  6.303e-02  -0.240 0.810537
    ## `n_tokens_title:kw_max_avg`                               -1.608e-02  2.986e-02  -0.539 0.590250
    ## `n_tokens_title:kw_avg_avg`                               -1.267e-02  1.246e-01  -0.102 0.919036
    ## `n_tokens_title:self_reference_min_shares`                 4.690e-02  3.280e-02   1.430 0.152852
    ## `n_tokens_title:self_reference_max_shares`                 5.212e-02  2.062e-02   2.528 0.011517
    ## `n_tokens_title:self_reference_avg_sharess`               -1.087e-01  5.130e-02  -2.119 0.034108
    ## `n_tokens_title:weekday_is_monday`                        -6.942e+01  1.571e+02  -0.442 0.658580
    ## `n_tokens_title:weekday_is_tuesday`                       -7.590e+01  1.535e+02  -0.494 0.620987
    ## `n_tokens_title:weekday_is_wednesday`                     -8.425e+01  1.543e+02  -0.546 0.585129
    ## `n_tokens_title:weekday_is_thursday`                      -1.531e+02  1.557e+02  -0.983 0.325730
    ## `n_tokens_title:weekday_is_friday`                        -1.382e+02  1.582e+02  -0.873 0.382584
    ## `n_tokens_title:weekday_is_saturday`                      -1.009e+02  1.930e+02  -0.523 0.600890
    ## `n_tokens_title:weekday_is_sunday`                                NA         NA      NA       NA
    ## `n_tokens_title:is_weekend`                                       NA         NA      NA       NA
    ## `n_tokens_title:LDA_00`                                    4.936e+02  4.050e+02   1.219 0.223069
    ## `n_tokens_title:LDA_01`                                    1.027e+02  5.118e+02   0.201 0.841010
    ## `n_tokens_title:LDA_02`                                   -1.339e+02  2.276e+02  -0.589 0.556159
    ## `n_tokens_title:LDA_03`                                    2.713e+02  3.972e+02   0.683 0.494621
    ## `n_tokens_title:LDA_04`                                           NA         NA      NA       NA
    ## `n_tokens_title:global_subjectivity`                       2.094e+02  5.499e+02   0.381 0.703413
    ## `n_tokens_title:global_sentiment_polarity`                 3.931e+02  1.314e+03   0.299 0.764847
    ## `n_tokens_title:global_rate_positive_words`               -1.201e+03  6.458e+03  -0.186 0.852463
    ## `n_tokens_title:global_rate_negative_words`               -4.256e+03  1.054e+04  -0.404 0.686452
    ## `n_tokens_title:rate_positive_words`                      -5.416e+02  7.649e+02  -0.708 0.478902
    ## `n_tokens_title:rate_negative_words`                              NA         NA      NA       NA
    ## `n_tokens_title:avg_positive_polarity`                    -8.978e+02  8.739e+02  -1.027 0.304277
    ## `n_tokens_title:min_positive_polarity`                    -2.235e+02  8.058e+02  -0.277 0.781517
    ## `n_tokens_title:max_positive_polarity`                     2.853e+02  2.458e+02   1.161 0.245803
    ## `n_tokens_title:avg_negative_polarity`                     7.906e+02  7.320e+02   1.080 0.280152
    ## `n_tokens_title:min_negative_polarity`                    -9.423e+01  2.456e+02  -0.384 0.701283
    ## `n_tokens_title:max_negative_polarity`                    -1.665e+03  7.527e+02  -2.211 0.027054
    ## `n_tokens_title:title_subjectivity`                       -1.474e+02  1.870e+02  -0.789 0.430424
    ## `n_tokens_title:title_sentiment_polarity`                  2.259e+02  1.696e+02   1.332 0.182920
    ## `n_tokens_title:abs_title_subjectivity`                    1.607e+02  2.348e+02   0.684 0.493758
    ## `n_tokens_title:abs_title_sentiment_polarity`              4.076e+02  2.865e+02   1.423 0.154850
    ## `n_tokens_content:n_unique_tokens`                        -1.975e+01  1.000e+01  -1.975 0.048354
    ## `n_tokens_content:n_non_stop_words`                               NA         NA      NA       NA
    ## `n_tokens_content:n_non_stop_unique_tokens`                7.679e+00  8.814e+00   0.871 0.383688
    ## `n_tokens_content:num_hrefs`                               3.998e-02  3.045e-02   1.313 0.189297
    ## `n_tokens_content:num_self_hrefs`                          8.005e-03  1.258e-01   0.064 0.949259
    ## `n_tokens_content:num_imgs`                               -1.340e-01  5.051e-02  -2.653 0.008008
    ## `n_tokens_content:num_videos`                              1.457e-02  1.868e-01   0.078 0.937858
    ## `n_tokens_content:average_token_length`                    1.068e+00  1.527e+00   0.699 0.484520
    ## `n_tokens_content:num_keywords`                           -2.096e-01  2.400e-01  -0.873 0.382500
    ## `n_tokens_content:kw_min_min`                             -6.186e-03  1.730e-02  -0.358 0.720713
    ## `n_tokens_content:kw_max_min`                             -1.949e-03  7.272e-04  -2.680 0.007380
    ## `n_tokens_content:kw_avg_min`                              5.589e-03  4.266e-03   1.310 0.190234
    ## `n_tokens_content:kw_min_max`                             -6.861e-05  3.113e-05  -2.204 0.027591
    ## `n_tokens_content:kw_max_max`                             -3.068e-06  5.567e-06  -0.551 0.581566
    ## `n_tokens_content:kw_avg_max`                             -2.958e-06  6.830e-06  -0.433 0.665012
    ## `n_tokens_content:kw_min_avg`                              1.326e-03  6.222e-04   2.131 0.033138
    ## `n_tokens_content:kw_max_avg`                              4.221e-04  2.875e-04   1.468 0.142122
    ## `n_tokens_content:kw_avg_avg`                             -7.470e-04  1.150e-03  -0.649 0.516167
    ## `n_tokens_content:self_reference_min_shares`               1.265e-04  3.107e-04   0.407 0.683930
    ## `n_tokens_content:self_reference_max_shares`               3.178e-04  2.057e-04   1.545 0.122326
    ## `n_tokens_content:self_reference_avg_sharess`             -7.179e-04  4.919e-04  -1.460 0.144485
    ## `n_tokens_content:weekday_is_monday`                      -1.294e+00  1.528e+00  -0.847 0.396917
    ## `n_tokens_content:weekday_is_tuesday`                     -5.936e-01  1.355e+00  -0.438 0.661424
    ## `n_tokens_content:weekday_is_wednesday`                    1.729e+00  1.400e+00   1.235 0.216767
    ## `n_tokens_content:weekday_is_thursday`                    -9.721e-02  1.358e+00  -0.072 0.942948
    ## `n_tokens_content:weekday_is_friday`                      -1.274e+00  1.467e+00  -0.869 0.385029
    ## `n_tokens_content:weekday_is_saturday`                    -2.219e+00  1.943e+00  -1.142 0.253501
    ## `n_tokens_content:weekday_is_sunday`                              NA         NA      NA       NA
    ## `n_tokens_content:is_weekend`                                     NA         NA      NA       NA
    ## `n_tokens_content:LDA_00`                                  5.673e+00  3.690e+00   1.537 0.124266
    ## `n_tokens_content:LDA_01`                                  4.729e+00  5.018e+00   0.942 0.346027
    ## `n_tokens_content:LDA_02`                                  1.730e+00  2.308e+00   0.750 0.453567
    ## `n_tokens_content:LDA_03`                                  2.235e+00  4.081e+00   0.548 0.583920
    ## `n_tokens_content:LDA_04`                                         NA         NA      NA       NA
    ## `n_tokens_content:global_subjectivity`                     3.070e-01  6.721e+00   0.046 0.963566
    ## `n_tokens_content:global_sentiment_polarity`              -2.862e+00  1.796e+01  -0.159 0.873367
    ## `n_tokens_content:global_rate_positive_words`              6.159e+01  7.269e+01   0.847 0.396919
    ## `n_tokens_content:global_rate_negative_words`              6.302e+01  1.321e+02   0.477 0.633240
    ## `n_tokens_content:rate_positive_words`                     3.145e-01  1.078e+01   0.029 0.976726
    ## `n_tokens_content:rate_negative_words`                            NA         NA      NA       NA
    ## `n_tokens_content:avg_positive_polarity`                  -3.351e+00  1.135e+01  -0.295 0.767912
    ## `n_tokens_content:min_positive_polarity`                   2.516e-01  9.921e+00   0.025 0.979770
    ## `n_tokens_content:max_positive_polarity`                  -3.332e+00  2.721e+00  -1.225 0.220802
    ##                                                              
    ## (Intercept)                                                  
    ## timedelta                                                    
    ## n_tokens_title                                               
    ## n_tokens_content                                             
    ## n_unique_tokens                                              
    ## n_non_stop_words                                             
    ## n_non_stop_unique_tokens                                     
    ## num_hrefs                                                    
    ## num_self_hrefs                                            .  
    ## num_imgs                                                     
    ## num_videos                                                   
    ## average_token_length                                         
    ## num_keywords                                                 
    ## kw_min_min                                                   
    ## kw_max_min                                                   
    ## kw_avg_min                                                   
    ## kw_min_max                                                   
    ## kw_max_max                                                   
    ## kw_avg_max                                                   
    ## kw_min_avg                                                   
    ## kw_max_avg                                                   
    ## kw_avg_avg                                                   
    ## self_reference_min_shares                                    
    ## self_reference_max_shares                                    
    ## self_reference_avg_sharess                                   
    ## weekday_is_monday                                         *  
    ## weekday_is_tuesday                                           
    ## weekday_is_wednesday                                      .  
    ## weekday_is_thursday                                          
    ## weekday_is_friday                                            
    ## weekday_is_saturday                                          
    ## weekday_is_sunday                                            
    ## is_weekend                                                   
    ## LDA_00                                                       
    ## LDA_01                                                       
    ## LDA_02                                                       
    ## LDA_03                                                       
    ## LDA_04                                                       
    ## global_subjectivity                                          
    ## global_sentiment_polarity                                    
    ## global_rate_positive_words                                .  
    ## global_rate_negative_words                                   
    ## rate_positive_words                                          
    ## rate_negative_words                                          
    ## avg_positive_polarity                                        
    ## min_positive_polarity                                        
    ## max_positive_polarity                                        
    ## avg_negative_polarity                                        
    ## min_negative_polarity                                        
    ## max_negative_polarity                                        
    ## title_subjectivity                                        ***
    ## title_sentiment_polarity                                     
    ## abs_title_subjectivity                                    .  
    ## abs_title_sentiment_polarity                                 
    ## `timedelta:n_tokens_title`                                   
    ## `timedelta:n_tokens_content`                                 
    ## `timedelta:n_unique_tokens`                                  
    ## `timedelta:n_non_stop_words`                                 
    ## `timedelta:n_non_stop_unique_tokens`                         
    ## `timedelta:num_hrefs`                                        
    ## `timedelta:num_self_hrefs`                                ** 
    ## `timedelta:num_imgs`                                      ** 
    ## `timedelta:num_videos`                                       
    ## `timedelta:average_token_length`                             
    ## `timedelta:num_keywords`                                  *  
    ## `timedelta:kw_min_min`                                    .  
    ## `timedelta:kw_max_min`                                       
    ## `timedelta:kw_avg_min`                                       
    ## `timedelta:kw_min_max`                                       
    ## `timedelta:kw_max_max`                                       
    ## `timedelta:kw_avg_max`                                       
    ## `timedelta:kw_min_avg`                                       
    ## `timedelta:kw_max_avg`                                       
    ## `timedelta:kw_avg_avg`                                       
    ## `timedelta:self_reference_min_shares`                     .  
    ## `timedelta:self_reference_max_shares`                     ** 
    ## `timedelta:self_reference_avg_sharess`                    .  
    ## `timedelta:weekday_is_monday`                                
    ## `timedelta:weekday_is_tuesday`                               
    ## `timedelta:weekday_is_wednesday`                             
    ## `timedelta:weekday_is_thursday`                              
    ## `timedelta:weekday_is_friday`                                
    ## `timedelta:weekday_is_saturday`                              
    ## `timedelta:weekday_is_sunday`                                
    ## `timedelta:is_weekend`                                       
    ## `timedelta:LDA_00`                                           
    ## `timedelta:LDA_01`                                           
    ## `timedelta:LDA_02`                                           
    ## `timedelta:LDA_03`                                           
    ## `timedelta:LDA_04`                                           
    ## `timedelta:global_subjectivity`                              
    ## `timedelta:global_sentiment_polarity`                        
    ## `timedelta:global_rate_positive_words`                       
    ## `timedelta:global_rate_negative_words`                    .  
    ## `timedelta:rate_positive_words`                              
    ## `timedelta:rate_negative_words`                              
    ## `timedelta:avg_positive_polarity`                            
    ## `timedelta:min_positive_polarity`                         .  
    ## `timedelta:max_positive_polarity`                            
    ## `timedelta:avg_negative_polarity`                            
    ## `timedelta:min_negative_polarity`                            
    ## `timedelta:max_negative_polarity`                            
    ## `timedelta:title_subjectivity`                               
    ## `timedelta:title_sentiment_polarity`                         
    ## `timedelta:abs_title_subjectivity`                           
    ## `timedelta:abs_title_sentiment_polarity`                     
    ## `n_tokens_title:n_tokens_content`                            
    ## `n_tokens_title:n_unique_tokens`                             
    ## `n_tokens_title:n_non_stop_words`                         .  
    ## `n_tokens_title:n_non_stop_unique_tokens`                    
    ## `n_tokens_title:num_hrefs`                                   
    ## `n_tokens_title:num_self_hrefs`                           .  
    ## `n_tokens_title:num_imgs`                                 *  
    ## `n_tokens_title:num_videos`                                  
    ## `n_tokens_title:average_token_length`                        
    ## `n_tokens_title:num_keywords`                                
    ## `n_tokens_title:kw_min_min`                                  
    ## `n_tokens_title:kw_max_min`                               *  
    ## `n_tokens_title:kw_avg_min`                               *  
    ## `n_tokens_title:kw_min_max`                                  
    ## `n_tokens_title:kw_max_max`                                  
    ## `n_tokens_title:kw_avg_max`                                  
    ## `n_tokens_title:kw_min_avg`                                  
    ## `n_tokens_title:kw_max_avg`                                  
    ## `n_tokens_title:kw_avg_avg`                                  
    ## `n_tokens_title:self_reference_min_shares`                   
    ## `n_tokens_title:self_reference_max_shares`                *  
    ## `n_tokens_title:self_reference_avg_sharess`               *  
    ## `n_tokens_title:weekday_is_monday`                           
    ## `n_tokens_title:weekday_is_tuesday`                          
    ## `n_tokens_title:weekday_is_wednesday`                        
    ## `n_tokens_title:weekday_is_thursday`                         
    ## `n_tokens_title:weekday_is_friday`                           
    ## `n_tokens_title:weekday_is_saturday`                         
    ## `n_tokens_title:weekday_is_sunday`                           
    ## `n_tokens_title:is_weekend`                                  
    ## `n_tokens_title:LDA_00`                                      
    ## `n_tokens_title:LDA_01`                                      
    ## `n_tokens_title:LDA_02`                                      
    ## `n_tokens_title:LDA_03`                                      
    ## `n_tokens_title:LDA_04`                                      
    ## `n_tokens_title:global_subjectivity`                         
    ## `n_tokens_title:global_sentiment_polarity`                   
    ## `n_tokens_title:global_rate_positive_words`                  
    ## `n_tokens_title:global_rate_negative_words`                  
    ## `n_tokens_title:rate_positive_words`                         
    ## `n_tokens_title:rate_negative_words`                         
    ## `n_tokens_title:avg_positive_polarity`                       
    ## `n_tokens_title:min_positive_polarity`                       
    ## `n_tokens_title:max_positive_polarity`                       
    ## `n_tokens_title:avg_negative_polarity`                       
    ## `n_tokens_title:min_negative_polarity`                       
    ## `n_tokens_title:max_negative_polarity`                    *  
    ## `n_tokens_title:title_subjectivity`                          
    ## `n_tokens_title:title_sentiment_polarity`                    
    ## `n_tokens_title:abs_title_subjectivity`                      
    ## `n_tokens_title:abs_title_sentiment_polarity`                
    ## `n_tokens_content:n_unique_tokens`                        *  
    ## `n_tokens_content:n_non_stop_words`                          
    ## `n_tokens_content:n_non_stop_unique_tokens`                  
    ## `n_tokens_content:num_hrefs`                                 
    ## `n_tokens_content:num_self_hrefs`                            
    ## `n_tokens_content:num_imgs`                               ** 
    ## `n_tokens_content:num_videos`                                
    ## `n_tokens_content:average_token_length`                      
    ## `n_tokens_content:num_keywords`                              
    ## `n_tokens_content:kw_min_min`                                
    ## `n_tokens_content:kw_max_min`                             ** 
    ## `n_tokens_content:kw_avg_min`                                
    ## `n_tokens_content:kw_min_max`                             *  
    ## `n_tokens_content:kw_max_max`                                
    ## `n_tokens_content:kw_avg_max`                                
    ## `n_tokens_content:kw_min_avg`                             *  
    ## `n_tokens_content:kw_max_avg`                                
    ## `n_tokens_content:kw_avg_avg`                                
    ## `n_tokens_content:self_reference_min_shares`                 
    ## `n_tokens_content:self_reference_max_shares`                 
    ## `n_tokens_content:self_reference_avg_sharess`                
    ## `n_tokens_content:weekday_is_monday`                         
    ## `n_tokens_content:weekday_is_tuesday`                        
    ## `n_tokens_content:weekday_is_wednesday`                      
    ## `n_tokens_content:weekday_is_thursday`                       
    ## `n_tokens_content:weekday_is_friday`                         
    ## `n_tokens_content:weekday_is_saturday`                       
    ## `n_tokens_content:weekday_is_sunday`                         
    ## `n_tokens_content:is_weekend`                                
    ## `n_tokens_content:LDA_00`                                    
    ## `n_tokens_content:LDA_01`                                    
    ## `n_tokens_content:LDA_02`                                    
    ## `n_tokens_content:LDA_03`                                    
    ## `n_tokens_content:LDA_04`                                    
    ## `n_tokens_content:global_subjectivity`                       
    ## `n_tokens_content:global_sentiment_polarity`                 
    ## `n_tokens_content:global_rate_positive_words`                
    ## `n_tokens_content:global_rate_negative_words`                
    ## `n_tokens_content:rate_positive_words`                       
    ## `n_tokens_content:rate_negative_words`                       
    ## `n_tokens_content:avg_positive_polarity`                     
    ## `n_tokens_content:min_positive_polarity`                     
    ## `n_tokens_content:max_positive_polarity`                     
    ##  [ reached getOption("max.print") -- omitted 1232 rows ]
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4513 on 4705 degrees of freedom
    ## Multiple R-squared:  0.3942, Adjusted R-squared:  0.2404 
    ## F-statistic: 2.564 on 1194 and 4705 DF,  p-value: < 2.2e-16

``` r
lmFit2Pred <- predict(lmFit2, newdata = worldTest)
lm2 <- postResample(lmFit2Pred, worldTest$shares)
```

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
#using cross validation
ncol(worldTrain)
```

    ## [1] 54

``` r
rfFit <- train(shares ~ ., data = worldTrain, method = "rf", 
               trControl = trainControl(method = "cv", number = 5), 
               tuneGrid = data.frame(mtry = 1:15))
rfFit$results
```

    ##    mtry     RMSE   Rsquared      MAE   RMSESD RsquaredSD    MAESD
    ## 1     1 5053.781 0.04056730 1867.355 621.2331 0.01787356 49.18585
    ## 2     2 5059.843 0.03661339 1894.188 606.7245 0.01242301 39.38592
    ## 3     3 5059.318 0.03912936 1914.917 597.8313 0.01750117 36.66609
    ## 4     4 5076.742 0.03581233 1931.292 594.4710 0.01465061 41.74159
    ## 5     5 5090.140 0.03439201 1951.806 574.8538 0.01582734 45.71873
    ## 6     6 5090.226 0.03611272 1957.351 572.5185 0.01814199 51.21125
    ## 7     7 5099.520 0.03566319 1968.176 570.7266 0.02101796 49.54556
    ## 8     8 5103.802 0.03451512 1969.499 568.5140 0.01843949 42.94706
    ## 9     9 5123.226 0.03157709 1987.715 567.7066 0.01727427 47.01208
    ## 10   10 5122.742 0.03309627 1986.520 559.1148 0.01864858 47.50034
    ## 11   11 5130.306 0.03203902 1994.092 557.6911 0.01758917 47.02215
    ## 12   12 5146.627 0.03110408 2005.927 548.5512 0.01964234 58.31478
    ## 13   13 5150.716 0.02999307 2006.834 555.3116 0.01767717 55.63093
    ## 14   14 5153.685 0.02954105 2006.172 557.6989 0.01861580 48.04886
    ## 15   15 5151.584 0.03175971 2010.750 552.1653 0.02234825 49.05812

``` r
rfFit$bestTune
```

    ##   mtry
    ## 1    1

``` r
rfPred <- predict(rfFit, newdata = worldTest)
rf <- postResample(rfPred, worldTest$shares)
```

*You’ll need to split the data into a training (70% of the data) and
test set (30% of the data). Use set.seed() to make things reproducible.*

*The goal is to create models for predicting the number of shares in
some way.*

*Each group member should contribute a linear regression model and an
ensemble tree-based model. As we are automating things, describing the
chosen model is tough, so no need to worry about that.*

*Both models should be chosen using cross-validation.*

## Comparison

**RINA or JESS**

*All four of the models should be compared on the test set and a winner
declared (this should be automated to be correct across all the created
documents).*

``` r
data.frame(linear2 = lm2, randomForest = rf)
```

    ##               linear2 randomForest
    ## RMSE     1.357836e+04 7.746580e+03
    ## Rsquared 7.552818e-04 1.849627e-02
    ## MAE      3.966029e+03 1.905182e+03

# Automation

**RINA or JESS**

*Once you’ve completed the above for a particular data channel, adapt
the code so that you can use a parameter in your build process. You
should be able to automatically generate an analysis report for each
data_channel_is\_ variable*

*You’ll end up with six total outputted documents.*
