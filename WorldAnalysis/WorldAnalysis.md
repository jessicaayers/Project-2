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

    ##  [1] "url"                           "timedelta"                     "n_tokens_title"                "n_tokens_content"             
    ##  [5] "n_unique_tokens"               "n_non_stop_words"              "n_non_stop_unique_tokens"      "num_hrefs"                    
    ##  [9] "num_self_hrefs"                "num_imgs"                      "num_videos"                    "average_token_length"         
    ## [13] "num_keywords"                  "data_channel_is_lifestyle"     "data_channel_is_entertainment" "data_channel_is_bus"          
    ## [17] "data_channel_is_socmed"        "data_channel_is_tech"          "data_channel_is_world"         "kw_min_min"                   
    ## [21] "kw_max_min"                    "kw_avg_min"                    "kw_min_max"                    "kw_max_max"                   
    ## [25] "kw_avg_max"                    "kw_min_avg"                    "kw_max_avg"                    "kw_avg_avg"                   
    ## [29] "self_reference_min_shares"     "self_reference_max_shares"     "self_reference_avg_sharess"    "weekday_is_monday"            
    ## [33] "weekday_is_tuesday"            "weekday_is_wednesday"          "weekday_is_thursday"           "weekday_is_friday"            
    ## [37] "weekday_is_saturday"           "weekday_is_sunday"             "is_weekend"                    "LDA_00"                       
    ## [41] "LDA_01"                        "LDA_02"                        "LDA_03"                        "LDA_04"                       
    ## [45] "global_subjectivity"           "global_sentiment_polarity"     "global_rate_positive_words"    "global_rate_negative_words"   
    ## [49] "rate_positive_words"           "rate_negative_words"           "avg_positive_polarity"         "min_positive_polarity"        
    ## [53] "max_positive_polarity"         "avg_negative_polarity"         "min_negative_polarity"         "max_negative_polarity"        
    ## [57] "title_subjectivity"            "title_sentiment_polarity"      "abs_title_subjectivity"        "abs_title_sentiment_polarity" 
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
set.seed(111)
train <- sample(1:nrow(onpdata_world), size = nrow(onpdata_world)*0.70) 
test <- dplyr::setdiff(1:nrow(onpdata_world), train)
worldTrain <- onpdata_world[train, ] 
worldTest <- onpdata_world[test, ]
```

*You should produce some basic (but meaningful) summary statistics and
plots about the training data you are working with (especially as it
relates to your response).*

``` r
summary(worldTrain$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41     827    1100    2283    1900  284700

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

# Automation

**RINA or JESS**

*Once you’ve completed the above for a particular data channel, adapt
the code so that you can use a parameter in your build process. You
should be able to automatically generate an analysis report for each
data_channel_is\_ variable*

*You’ll end up with six total outputted documents.*
