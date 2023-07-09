# Project-2

## Purpose:

The purpose of this repo is to explore different types of models predicting the number of shares from the Online News Popularity data. Six different files are created. Each one is unique to the type of data channel. Summary plots are created along with the creation of two different linear models, a boosted tree model, and a random forest model. At the end of the analysis, the model with the smallest RMSE (root mean squared error) is chosen to be the best fitting model. 

## Packages Used:

`library(tidyverse)`

`library(rmarkdown)`

`library(caret)`

`library(corrplot)`

`library(gbm)`

`library(randomForest)`

## Links Used:

[Lifestyle link here](https://htmlpreview.github.io/?https://github.com/jessicaayers/Project-2/blob/main/LifestyleAnalysis.md)

[Entertainment link here](https://htmlpreview.github.io/?https://github.com/jessicaayers/Project-2/blob/main/EntertainmentAnalysis.md)

[Bus link here](https://htmlpreview.github.io/?https://github.com/jessicaayers/Project-2/blob/main/BusAnalysis.md)

[Socmed link here](https://htmlpreview.github.io/?https://github.com/jessicaayers/Project-2/blob/main/SocmedAnalysis.md)

[Tech link here](https://htmlpreview.github.io/?https://github.com/jessicaayers/Project-2/blob/main/TechAnalysis.md)

[World link here](https://htmlpreview.github.io/?https://github.com/jessicaayers/Project-2/blob/main/WorldAnalysis.md)

## Code: 

First the Online News Popularity data was read in and the channel variable was created. The parameters used to automate the R Markdown file were based on the new channel variable. The render function was then looped through all 6 different channels along with subsetting the data as well.

```{r}
onpdata <- read_csv("../OnlineNewsPopularity/OnlineNewsPopularity.csv")

for(i in 1:nrow(onpdata)){
  if(onpdata$data_channel_is_lifestyle[i] == 1){
    onpdata$channel[i] <- "Lifestyle"
  }
  else if(onpdata$data_channel_is_entertainment[i] == 1){
    onpdata$channel[i] <- "Entertainment"
  }
  else if(onpdata$data_channel_is_bus[i] == 1){
    onpdata$channel[i] <- "Bus"
  }
  else if(onpdata$data_channel_is_socmed[i] == 1){
    onpdata$channel[i] <- "Socmed"
  }
  else if(onpdata$data_channel_is_tech[i] == 1){
    onpdata$channel[i] <- "Tech"
  }
  else if(onpdata$data_channel_is_world[i] == 1){
    onpdata$channel[i] <- "World"
  }
}
```

```{r}
channelIDs <- unique(onpdata$channel)
output_file <- paste0(channelIDs, "Analysis.md")
params <- lapply(channelIDs, FUN = function(x){list(channel=x)})
analysis <- tibble(output_file, params)
```

```{r}
for(i in 1:6){
onpdata_subset <- subset(onpdata, channel == params[[i]])
render(input = "WorldAnalysis.Rmd", output_file = output_file[i], params = params[i])
}
```

